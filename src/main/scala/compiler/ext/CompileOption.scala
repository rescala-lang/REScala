package compiler.ext

import clangast.{CASTNode, given}
import clangast.decl.*
import clangast.expr.binaryop.{CAndExpr, CEqualsExpr, CNotEqualsExpr, COrExpr}
import clangast.expr.unaryop.{CDerefExpr, CNotExpr}
import clangast.expr.*
import clangast.stmt.{CCompoundStmt, CIfStmt, CReturnStmt, CStmt}
import clangast.stubs.StdBoolH
import clangast.types.*
import compiler.CompilerCascade
import compiler.base.*
import compiler.base.CompileType.typeArgs
import compiler.context.{RecordDeclTC, TranslationContext, ValueDeclTC}

import scala.quoted.*

object CompileOption extends DefinitionPC with TermPC with SelectPC with ApplyPC with MatchPC with TypePC with StringPC {
  private def compileValDefToCVarDeclImpl(using Quotes)(using ctx: RecordDeclTC & ValueDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = {
      import quotes.reflect.*

      {
        case ValDef(name, tpt, Some(Ident("None"))) =>
          val init = CCallExpr(CDeclRefExpr(getNoneCreator(tpt.tpe)), List())
          val decl = CVarDecl(name, cascade.dispatch(_.compileTypeRepr)(tpt.tpe), Some(init))
          ctx.nameToDecl.put(name, decl)
          decl
      }
    }

  override def compileValDefToCVarDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = ensureCtx[RecordDeclTC & ValueDeclTC](compileValDefToCVarDeclImpl)

  private def compileTermImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Term, CASTNode] = {
      import quotes.reflect.*

      {
        case TypeApply(Select(Ident("Option"), "empty"), List(wrappedType)) =>
          CCallExpr(
            CDeclRefExpr(getNoneCreator(TypeRepr.of[Option].appliedTo(wrappedType.tpe))),
            List()
          )
      }
    }

  override def compileTerm(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CASTNode] = ensureCtx[RecordDeclTC](compileTermImpl)

  private def compileSelectImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = {
      import quotes.reflect.*

      {
        case Select(opt, "get") if opt.tpe <:< TypeRepr.of[Option[?]] =>
          val valField = getValField(getOptionRecordDecl(opt.tpe))
          CMemberExpr(cascade.dispatch(_.compileTermToCExpr)(opt), valField)
        case Select(opt, "isDefined") if opt.tpe <:< TypeRepr.of[Option[?]] =>
          val definedField = getDefinedField(getOptionRecordDecl(opt.tpe))
          CMemberExpr(cascade.dispatch(_.compileTermToCExpr)(opt), definedField)
        case Select(opt, "isEmpty") if opt.tpe <:< TypeRepr.of[Option[?]] =>
          val definedField = getDefinedField(getOptionRecordDecl(opt.tpe))
          CNotExpr(CMemberExpr(cascade.dispatch(_.compileTermToCExpr)(opt), definedField))
      }
    }

  override def compileSelect(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = ensureCtx[RecordDeclTC](compileSelectImpl)

  private def compileApplyImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*

      {
        case Apply(TypeApply(Select(Ident("Some"), "apply"), _), List(inner)) =>
          CCallExpr(
            CDeclRefExpr(getSomeCreator(TypeRepr.of[Option].appliedTo(inner.tpe))),
            List(cascade.dispatch(_.compileTermToCExpr)(inner))
          )
        case Apply(TypeApply(Select(opt, "getOrElse"), _), List(defaultVal)) =>
          CCallExpr(
            CDeclRefExpr(getGetOrElse(opt.tpe)),
            List(
              cascade.dispatch(_.compileTermToCExpr)(opt),
              cascade.dispatch(_.compileTermToCExpr)(defaultVal)
            )
          )
      }
    }

  override def compileApply(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[RecordDeclTC](compileApplyImpl)

  private def compileEqualsImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] = {
      import quotes.reflect.*

      {
        case (leftExpr, leftType, rightExpr, _) if leftType <:< TypeRepr.of[Product] =>
          CCallExpr(
            CDeclRefExpr(getOptionEquals(leftType)),
            List(leftExpr, rightExpr)
          )
      }
    }

  override def compileEquals(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] =
      ensureCtx[RecordDeclTC](compileEqualsImpl)

  private def compilePatternImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] = {
      import quotes.reflect.*

      {
        case (Ident("None"), prefix, prefixType) =>
          val recordDecl = getOptionRecordDecl(prefixType)
          val cond = CNotExpr(CMemberExpr(prefix, getDefinedField(recordDecl)))
          (Some(cond), List())
        case (TypedOrTest(Unapply(_, _, List(subPattern)), _), prefix, prefixType) if prefixType <:< TypeRepr.of[Option[?]] =>
          val recordDecl = getOptionRecordDecl(prefixType)
          val subPrefix = CMemberExpr(prefix, getValField(recordDecl))
          val typeArgs(List(subPrefixType)) = prefixType.widen

          val definedCond = CMemberExpr(prefix, getDefinedField(recordDecl))

          val (subCond, subDecls) = cascade.dispatch(_.compilePattern)(subPattern, subPrefix, subPrefixType)

          val combinedCond = CompileMatch.combineCond(Some(definedCond), subCond)

          (combinedCond, subDecls)
      }
    }

  override def compilePattern(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] =
      ensureCtx[RecordDeclTC](compilePatternImpl)

  private def compileTypeReprImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Option[?]] =>
          CRecordType(getOptionRecordDecl(tpe))
      }
    }

  override def compileTypeRepr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC](compileTypeReprImpl)

  override def typeName(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Option[?]] => cascade.dispatch(_.classTypeName)(tpe)
      }
    }

  def compilePrintImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Option[?]] =>
          CCallExpr(CDeclRefExpr(getOptionPrinter(tpe)), List(expr))
      }
    }

  override def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC](compilePrintImpl)

  private def getOptionRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CRecordDecl = {
    ctx.nameToRecordDecl.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe), compileOptionTypeToCRecordDecl(tpe))
  }

  private def compileOptionTypeToCRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): CRecordDecl = {
    import quotes.reflect.*

    val typeArgs(List(wrappedType)) = tpe

    val valField = CFieldDecl("val", cascade.dispatch(_.compileTypeRepr)(wrappedType))
    val definedField = CFieldDecl("defined", StdBoolH.bool)

    CRecordDecl("Option_" + cascade.dispatch(_.typeName)(wrappedType), List(valField, definedField))
  }

  private def getValField(recordDecl: CRecordDecl): CFieldDecl = recordDecl.getField("val")

  private def getDefinedField(recordDecl: CRecordDecl): CFieldDecl = recordDecl.getField("defined")

  private val CREATE_NONE = "CREATE_NONE"
  private val CREATE_SOME = "CREATE_SOME"
  private val GET_OR_ELSE = "GET_OR_ELSE"
  private val EQUALS = "EQUALS"
  private val PRINT = "PRINT"

  private def getNoneCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE_NONE, buildNoneCreator(getOptionRecordDecl(tpe)))
  }

  private def getSomeCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE_SOME, buildSomeCreator(getOptionRecordDecl(tpe)))
  }

  private def buildNoneCreator(recordDecl: CRecordDecl)(using ctx: TranslationContext): CFunctionDecl = {
    val name = "createNone_" + recordDecl.name

    val optDecl = CVarDecl(
      "opt",
      recordDecl.getTypeForDecl,
      Some(CDesignatedInitExpr(List(
        "defined" -> CFalseLiteral
      )))
    )

    val body = CCompoundStmt(List(
      optDecl,
      CReturnStmt(Some(CDeclRefExpr(optDecl)))
    ))

    CFunctionDecl(name, List(), recordDecl.getTypeForDecl, Some(body))
  }

  private def buildSomeCreator(recordDecl: CRecordDecl)(using ctx: TranslationContext): CFunctionDecl = {
    val name = "createSome_" + recordDecl.name

    val valField = getValField(recordDecl)

    val valParam = CParmVarDecl("val", valField.declaredType)

    val optDecl = CVarDecl(
      "opt",
      recordDecl.getTypeForDecl,
      Some(CDesignatedInitExpr(List(
        "val" -> CDeclRefExpr(valParam),
        "defined" -> CTrueLiteral
      )))
    )

    val body = CCompoundStmt(List(
      optDecl,
      CReturnStmt(Some(CDeclRefExpr(optDecl)))
    ))

    CFunctionDecl(name, List(valParam), recordDecl.getTypeForDecl, Some(body))
  }

  private def getGetOrElse(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> GET_OR_ELSE, buildGetOrElse(getOptionRecordDecl(tpe)))
  }

  private def buildGetOrElse(recordDecl: CRecordDecl)(using ctx: TranslationContext): CFunctionDecl = {
    val name = "getOrElse_" + recordDecl.name

    val valField = getValField(recordDecl)
    val definedField = getDefinedField(recordDecl)

    val optParam = CParmVarDecl("opt", recordDecl.getTypeForDecl)
    val defaultValParam = CParmVarDecl("defaultVal", valField.declaredType)

    val body = CCompoundStmt(List(
      CConditionalOperator(
        CMemberExpr(CDeclRefExpr(optParam), definedField),
        CMemberExpr(CDeclRefExpr(optParam), valField),
        CDeclRefExpr(defaultValParam)
      )
    ))

    CFunctionDecl(name, List(optParam, defaultValParam), valField.declaredType, Some(body))
  }

  private def getOptionEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> EQUALS, buildOptionEquals(tpe))
  }

  private def buildOptionEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getOptionRecordDecl(tpe)
    val valField = getValField(recordDecl)
    val definedField = getDefinedField(recordDecl)
    val typeArgs(List(wrappedType)) = tpe.widen

    val name = "equals_" + recordDecl.name

    val paramLeft = CParmVarDecl("left", recordDecl.getTypeForDecl)
    val paramRight = CParmVarDecl("right", recordDecl.getTypeForDecl)

    val equalsExpr = COrExpr(
      CAndExpr(
        CNotExpr(CMemberExpr(CDeclRefExpr(paramLeft), definedField)),
        CNotExpr(CMemberExpr(CDeclRefExpr(paramRight), definedField))
      ),
      CAndExpr(
        CAndExpr(
          CMemberExpr(CDeclRefExpr(paramLeft), definedField),
          CMemberExpr(CDeclRefExpr(paramRight), definedField)
        ),
        cascade.dispatch(_.compileEquals)(
          CMemberExpr(CDeclRefExpr(paramLeft), valField),
          wrappedType,
          CMemberExpr(CDeclRefExpr(paramRight), valField),
          wrappedType,
        )
      )
    )

    val body = CCompoundStmt(List(CReturnStmt(Some(equalsExpr))))

    CFunctionDecl(name, List(paramLeft, paramRight), StdBoolH.bool, Some(body))
  }

  private def getOptionPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> PRINT, buildOptionPrinter(tpe))
  }

  private def buildOptionPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getOptionRecordDecl(tpe)
    val valField = getValField(recordDecl)
    val definedField = getDefinedField(recordDecl)
    val typeArgs(List(wrappedType)) = tpe.widen

    val name = "print_" + recordDecl.name

    val optParam = CParmVarDecl("opt", recordDecl.getTypeForDecl)

    val body = CCompoundStmt(List(
      CIfStmt(
        CMemberExpr(CDeclRefExpr(optParam), definedField),
        CCompoundStmt(List(
          CompileString.printf("Some("),
          cascade.dispatch(_.compilePrint)(
            CMemberExpr(CDeclRefExpr(optParam), valField),
            wrappedType
          ),
          CompileString.printf(")")
        )),
        Some(CompileString.printf("None"))
      )
    ))

    CFunctionDecl(name, List(optParam), CVoidType, Some(body))
  }
}
