package compiler.ext

import clangast.given
import clangast.decl.*
import clangast.expr.binaryop.{CAndExpr, CNotEqualsExpr}
import clangast.expr.unaryop.CNotExpr
import clangast.expr.*
import clangast.stmt.{CCompoundStmt, CIfStmt, CReturnStmt, CStmt}
import clangast.types.{CBoolType, CRecordType, CType}
import compiler.CompilerCascade
import compiler.base.*
import compiler.context.{RecordDeclTC, TranslationContext}

import scala.quoted.*

object CompileProduct extends DefinitionPC with SelectPC with ApplyPC with MatchPC with TypePC {
  private def compileValDefToCVarDeclImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = {
      import quotes.reflect.*

      {
        case ValDef(name, tpt, Some(apply@Apply(Select(_, "apply"), l))) if isProductApply(apply) =>
          val recordDecl = getProductRecordDecl(apply.tpe)
          val init = CDesignatedInitExpr(recordDecl.fields.map(_.name) zip l.map(cascade.dispatch(_.compileTermToCExpr)))
          CVarDecl(name, cascade.dispatch(_.compileTypeRepr)(tpt.tpe), Some(init))
      }
    }

  override def compileValDefToCVarDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = ensureCtx[RecordDeclTC](compileValDefToCVarDeclImpl)

  private def compileSelectImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = {
      import quotes.reflect.*

      {
        case Select(qualifier, name) if isProductFieldAccess(qualifier, name) =>
          val recordDecl = getProductRecordDecl(qualifier.tpe)

          CMemberExpr(cascade.dispatch(_.compileTermToCExpr)(qualifier), recordDecl.fields.find(_.name.equals(name)).get)
      }
    }

  override def compileSelect(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = ensureCtx[RecordDeclTC](compileSelectImpl)

  private def compileApplyImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*

      {
        case apply @ Apply(Select(_, "apply"), l) if isProductApply(apply) =>
          CCallExpr(CDeclRefExpr(getProductCreator(apply.tpe)), l.map(cascade.dispatch(_.compileTermToCExpr)))
        case apply @ Apply(TypeApply(Select(_, "apply"), _), l) if isProductApply(apply) =>
          CCallExpr(CDeclRefExpr(getProductCreator(apply.tpe)), l.map(cascade.dispatch(_.compileTermToCExpr)))
        case Apply(Select(left, "=="), List(right)) if left.tpe <:< TypeRepr.of[Product] =>
          CCallExpr(
            CDeclRefExpr(getProductEquals(left.tpe)),
            List(
              cascade.dispatch(_.compileTermToCExpr)(left),
              cascade.dispatch(_.compileTermToCExpr)(right)
            )
          )
        case Apply(Select(left, "!="), List(right)) if left.tpe <:< TypeRepr.of[Product] =>
          CNotExpr(
            CCallExpr(
              CDeclRefExpr(getProductEquals(left.tpe)),
              List(
                cascade.dispatch(_.compileTermToCExpr)(left),
                cascade.dispatch(_.compileTermToCExpr)(right)
              )
            )
          )
      }
    }

  override def compileApply(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[RecordDeclTC](compileApplyImpl)

  private def compilePatternImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] = {
      import quotes.reflect.*

      {
        case (Unapply(_, _, subPatterns), prefix, prefixType) if prefixType <:< TypeRepr.of[Product] =>
          val fieldSymbols = prefixType.classSymbol.get.caseFields.filter(_.isValDef)
          val recordDecl = getProductRecordDecl(prefixType)
          val subPrefixes = fieldSymbols.map(fs => CMemberExpr(prefix, recordDecl.fields.find(f => fs.name.strip().equals(f.name)).get))
          val subPrefixTypes = fieldSymbols.map(prefixType.memberType)

          (subPatterns zip (subPrefixes zip subPrefixTypes)).foldLeft((Option.empty[CExpr], List.empty[CVarDecl])) {
            case ((cond, decls), (subPattern, (subPrefix, subPrefixType))) =>
              val (subCond, subDecls) = cascade.dispatch(_.compilePattern)(subPattern, subPrefix, subPrefixType)

              val combinedCond = (cond, subCond) match {
                case (None, _) => subCond
                case (_, None) => cond
                case (Some(c1), Some(c2)) => Some(CAndExpr(c1, c2))
              }

              (combinedCond, subDecls ++ decls)
          }
      }
    }

  override def compilePattern(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] =
      ensureCtx[RecordDeclTC](compilePatternImpl)

  private def compileTypeReprImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = {
      import quotes.reflect.*
    
      {
        case tpe if tpe <:< TypeRepr.of[Product] =>
          CRecordType(getProductRecordDecl(tpe))
      }
    }

  override def compileTypeRepr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC](compileTypeReprImpl)

  override def typeName(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Product] => cascade.dispatch(_.classTypeName)(tpe)
      }
    }

  private def isProductApply(using Quotes)(apply: quotes.reflect.Apply): Boolean = {
    import quotes.reflect.*

    apply match {
      case Apply(Select(i, "apply"), _) =>
        (apply.tpe <:< TypeRepr.of[Product]) && (apply.tpe.classSymbol.get == i.tpe.classSymbol.get.companionClass)
      case Apply(TypeApply(Select(i, "apply"), _), _) =>
        (apply.tpe <:< TypeRepr.of[Product]) && (apply.tpe.classSymbol.get == i.tpe.classSymbol.get.companionClass)
      case _ => false
    }
  }

  private def getProductRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CRecordDecl = {
    ctx.getOrElseUpdateRecordDecl(cascade.dispatch(_.typeName)(tpe), compileProductTypeToCRecordDecl(tpe))
  }

  private def compileProductTypeToCRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): CRecordDecl = {
    import quotes.reflect.*

    val classSymbol = tpe.classSymbol.get

    val fields = classSymbol.caseFields.collect {
      case symbol if symbol.isValDef =>
        CFieldDecl(symbol.name.strip(), cascade.dispatch(_.compileTypeRepr)(tpe.memberType(symbol)))
    }

    CRecordDecl(cascade.dispatch(_.typeName)(tpe), fields)
  }

  private def isProductFieldAccess(using Quotes)(term: quotes.reflect.Term, name: String): Boolean = {
    import quotes.reflect.*

    (term.tpe <:< TypeRepr.of[Product]) && term.tpe.classSymbol.get.caseFields.exists(_.name.equals(name))
  }

  private def getProductCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.nameToRecordCreator.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe), buildProductCreator(getProductRecordDecl(tpe)))
  }

  private def buildProductCreator(recordDecl: CRecordDecl): CFunctionDecl = {
    val name = "create_" + recordDecl.name

    val parameters = recordDecl.fields.map {
      case CFieldDecl(name, declaredType) => CParmVarDecl(name, declaredType)
    }

    val returnType = CRecordType(recordDecl)

    val temp = CVarDecl(
      "temp",
      CRecordType(recordDecl),
      Some(CDesignatedInitExpr(
        parameters.map { p => (p.name, CDeclRefExpr(p)) }
      ))
    )
    val body = CCompoundStmt(List(
      temp,
      CReturnStmt(Some(CDeclRefExpr(temp)))
    ))

    CFunctionDecl(name, parameters, returnType, Some(body))
  }

  private def getProductEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using RecordDeclTC, CompilerCascade): CFunctionDecl =
    getProductEquals(getProductRecordDecl(tpe))

  private def getProductEquals(recordDecl: CRecordDecl)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.nameToRecordEquals.getOrElseUpdate(recordDecl.name, buildProductEquals(recordDecl))
  }

  private def buildProductEquals(recordDecl: CRecordDecl)(using RecordDeclTC, CompilerCascade): CFunctionDecl = {
    val name = "equals_" + recordDecl.name

    val paramLeft = CParmVarDecl("left", CRecordType(recordDecl))
    val paramRight = CParmVarDecl("right", CRecordType(recordDecl))
    val parameters = List(paramLeft, paramRight)

    val returnType = CBoolType

    val comparisons: List[CStmt] = recordDecl.fields.map { f =>
      val cond = f.declaredType.unqualType match {
        case CRecordType(rd) =>
          val eq = getProductEquals(rd)
          CNotExpr(
            CCallExpr(
              CDeclRefExpr(eq),
              List(
                CMemberExpr(CDeclRefExpr(paramLeft), f),
                CMemberExpr(CDeclRefExpr(paramRight), f)
              )
            )
          )
        case _ =>
          CNotEqualsExpr(
            CMemberExpr(CDeclRefExpr(paramLeft), f),
            CMemberExpr(CDeclRefExpr(paramRight), f)
          )
      }

      CIfStmt(
        cond,
        CReturnStmt(Some(CFalseLiteral))
      )
    }
    val body = CCompoundStmt(comparisons.appended(CReturnStmt(Some(CTrueLiteral))))

    CFunctionDecl(name, parameters, returnType, Some(body))
  }
}
