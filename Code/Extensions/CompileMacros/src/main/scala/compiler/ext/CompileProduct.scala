package compiler.ext

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.binaryop.*
import clangast.expr.unaryop.*
import clangast.expr.*
import clangast.stmt.*
import clangast.stubs.{StdBoolH, StdIOH, StdLibH, StringH}
import clangast.types.*
import compiler.CompilerCascade
import compiler.base.*
import compiler.base.CompileType.typeArgs
import compiler.base.CompileDataStructure.{release, retain}
import compiler.context.{RecordDeclTC, TranslationContext}

import scala.quoted.*

object CompileProduct extends SelectPC with ApplyPC with MatchPC with TypePC with DataStructurePC with StringPC {
  private def compileSelectImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = {
      import quotes.reflect.*

      {
        case Select(qualifier, name) if isProductFieldAccess(qualifier, name) =>
          CMemberExpr(cascade.dispatch(_.compileTermToCExpr)(qualifier), name)
      }
    }

  override def compileSelect(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = ensureCtx[RecordDeclTC](compileSelectImpl)

  private def compileApplyImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*

      {
        case apply @ Apply(Select(_, "apply"), l) if isProductApply(apply) =>
          CCallExpr(getProductCreator(apply.tpe).ref, l.map(cascade.dispatch(_.compileTermToCExpr)))
        case apply @ Apply(TypeApply(Select(_, "apply"), _), l) if isProductApply(apply) =>
          CCallExpr(getProductCreator(apply.tpe).ref, l.map(cascade.dispatch(_.compileTermToCExpr)))
        case Apply(Apply(TypeApply(Ident("deepCopy"), _), List(prod)), List()) if prod.tpe <:< TypeRepr.of[Product] =>
          CCallExpr(
            getProductDeepCopy(prod.tpe).ref,
            List(cascade.dispatch(_.compileTermToCExpr)(prod))
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
            getProductEquals(leftType).ref,
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
        case (Unapply(_, _, subPatterns), prefix, prefixType) if prefixType <:< TypeRepr.of[Product] =>
          val fieldSym = fieldSymbols(prefixType)
          val subPrefixes = fieldSym.map(fs => CMemberExpr(prefix, fs.name.strip()))
          val subPrefixTypes = fieldSym.map(prefixType.memberType)

          (subPatterns zip (subPrefixes zip subPrefixTypes)).foldLeft((Option.empty[CExpr], List.empty[CVarDecl])) {
            case ((cond, decls), (subPattern, (subPrefix, subPrefixType))) =>
              val (subCond, subDecls) = cascade.dispatch(_.compilePattern)(subPattern, subPrefix, subPrefixType)

              val combinedCond = CompileMatch.combineCond(cond, subCond)

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
          getRecordDecl(tpe).getTypeForDecl
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

  private def defaultValueImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CExpr] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Product] && hasDefaultValue(tpe) =>
          CCallExpr(
            getProductCreator(tpe).ref,
            fieldSymbols(tpe).map(tpe.memberType).map(cascade.dispatch(_.defaultValue))
          )
      }
    }

  override def defaultValue(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CExpr] = ensureCtx[RecordDeclTC](defaultValueImpl)

  override def compileTypeToCRecordDecl(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Product] =>
          val fields = fieldSymbols(tpe).map { symbol =>
            CFieldDecl(symbol.name.strip(), cascade.dispatch(_.compileTypeRepr)(tpe.memberType(symbol)))
          }

          val refCountFieldDecl =
            if cascade.dispatch(_.usesRefCount)(tpe) then
              List(CFieldDecl(refCountField, CPointerType(CIntegerType)))
            else Nil

          CRecordDecl(cascade.dispatch(_.typeName)(tpe), fields ++ refCountFieldDecl)
      }
    }

  override def usesRefCount(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Product] =>
          val typeArgs(fieldTypes) = tpe.widen
          fieldTypes.exists(cascade.dispatch(_.usesRefCount))
      }
    }

  private def compileFreeImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Product] =>
          val recordDecl = getRecordDecl(tpe)
          val typeArgs(fieldTypes) = tpe.widen

          val releaseElems: List[CStmt] = recordDecl.fields.zip(fieldTypes).collect {
            case (f, t) if cascade.dispatch(_.usesRefCount)(t) =>
              release(CMemberExpr(expr, f.name), t, CFalseLiteral).get
          }

          val freeRefCount = CCallExpr(StdLibH.free.ref, List(CMemberExpr(expr, refCountField)))

          CCompoundStmt(releaseElems :+ freeRefCount)
      }
    }

  override def compileFree(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = ensureCtx[RecordDeclTC](compileFreeImpl)

  private def compileDeepCopyImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Product] && cascade.dispatch(_.usesRefCount)(tpe) =>
          getProductDeepCopy(tpe)
      }
    }

  override def compileDeepCopy(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileDeepCopyImpl)

  private def compilePrintImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Product] =>
          CCallExpr(getProductPrinter(tpe).ref, List(expr))
      }
    }

  override def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC](compilePrintImpl)

  def compileToStringImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Product] =>
          CCallExpr(getProductToString(tpe).ref, List(expr))
      }
    }

  override def compileToString(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = ensureCtx[RecordDeclTC](compileToStringImpl)

  override def hasInjectiveToString(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Product] =>
          val typeArgs(fieldTypes) = tpe.widen
          fieldTypes.forall(cascade.dispatch(_.hasInjectiveToString))
      }
    }

  private val CREATE = "CREATE"
  private val EQUALS = "EQUALS"
  private val PRINT = "PRINT"
  private val TO_STRING = "TO_STRING"

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

  private def fieldSymbols(using Quotes)(tpe: quotes.reflect.TypeRepr): List[quotes.reflect.Symbol] =
    tpe.classSymbol.get.caseFields.filter(_.isValDef)

  private def hasDefaultValue(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): Boolean =
    !cascade.dispatch(_.usesRefCount)(tpe) &&
      fieldSymbols(tpe).map(tpe.memberType).forall(CompileType.hasDefaultValue)

  private def isProductFieldAccess(using Quotes)(term: quotes.reflect.Term, name: String): Boolean = {
    import quotes.reflect.*

    (term.tpe <:< TypeRepr.of[Product]) && fieldSymbols(term.tpe).exists(_.name.strip().equals(name))
  }

  private def getProductCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE, buildProductCreator(tpe))
  }

  private def buildProductCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    val recordDecl = getRecordDecl(tpe)
    val name = "create_" + recordDecl.name

    val parameters = fieldSymbols(tpe).map { fs =>
      val fieldName = fs.name.strip()
      CParmVarDecl(fieldName, recordDecl.getField(fieldName).declaredType)
    }

    val prodDecl = CVarDecl(
      "prod",
      recordDecl.getTypeForDecl,
      Some(CDesignatedInitExpr(
        (fieldSymbols(tpe) zip parameters).map {
          (fs: quotes.reflect.Symbol, p: CParmVarDecl) => (p.name, retain(p.ref, tpe.memberType(fs)))
        } ++ allocRefCount(tpe)
      ))
    )
    val body = CCompoundStmt(List(
      prodDecl,
      CReturnStmt(Some(prodDecl.ref))
    ))

    CFunctionDecl(name, parameters, recordDecl.getTypeForDecl, Some(body))
  }

  private def getProductEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(getRecordDecl(tpe).name -> EQUALS, buildProductEquals(tpe))
  }

  private def buildProductEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    val recordDecl = getRecordDecl(tpe)

    val name = "equals_" + recordDecl.name

    val paramLeft = CParmVarDecl("left", recordDecl.getTypeForDecl)
    val paramRight = CParmVarDecl("right", recordDecl.getTypeForDecl)
    val parameters = List(paramLeft, paramRight)

    val memberEquals: List[CExpr] = fieldSymbols(tpe).map { symbol =>
      val memberType = tpe.memberType(symbol)
      cascade.dispatch(_.compileEquals)(
        CMemberExpr(paramLeft.ref, symbol.name.strip()),
        memberType,
        CMemberExpr(paramRight.ref, symbol.name.strip()),
        memberType
      )
    }

    val equalsExpr = memberEquals.reduceOption(CAndExpr.apply).getOrElse(CTrueLiteral)

    val body = CCompoundStmt(List(CReturnStmt(Some(equalsExpr))))

    CFunctionDecl(name, parameters, StdBoolH.bool, Some(body))
  }

  private def getProductDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> DEEP_COPY, buildProductDeepCopy(tpe))
  }

  private def buildProductDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)

    val name = "deepCopy_" + recordDecl.name
    val productParam = CParmVarDecl("rec", recordDecl.getTypeForDecl)

    val copy = CCallExpr(
      getProductCreator(tpe).ref,
      fieldSymbols(tpe) map { fs =>
        val fieldType = tpe.memberType(fs)
        CompileDataStructure.deepCopy(
          CMemberExpr(productParam.ref, fs.name.strip()),
          fieldType
        )
      }
    )

    val body = CCompoundStmt(List(
      CReturnStmt(Some(copy))
    ))

    CFunctionDecl(
      name,
      List(productParam),
      recordDecl.getTypeForDecl,
      Some(body)
    )
  }

  private def getProductPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> PRINT, buildProductPrinter(tpe))
  }

  private def buildProductPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(fieldTypes) = tpe.widen

    val name = "print_" + recordDecl.name
    val productParam = CParmVarDecl("rec", recordDecl.getTypeForDecl)

    val printFields = recordDecl.fields.zip(fieldTypes).flatMap { (f, t) =>
      val printField = cascade.dispatch(_.compilePrint)(
        CMemberExpr(productParam.ref, f.name),
        t
      )

      List[CStmt](
        CompileString.printf(", "),
        printField
      )
    }.tail

    val body = CCompoundStmt(List(
      CompileString.printf(if recordDecl.name.startsWith("Tuple") then "(" else tpe.classSymbol.get.name + "("),
      CCompoundStmt(printFields),
      CompileString.printf(")")
    ))

    CFunctionDecl(
      name,
      List(productParam),
      CVoidType,
      Some(body)
    )
  }

  private def getProductToString(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> TO_STRING, buildProductToString(tpe))
  }

  private def buildProductToString(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(fieldTypes) = tpe.widen

    val isTuple = recordDecl.name.startsWith("Tuple")

    val name = "toString_" + recordDecl.name
    val productParam = CParmVarDecl("rec", recordDecl.getTypeForDecl)

    val fieldStringsDecl = CVarDecl(
      "fieldStrings",
      CArrayType(CPointerType(CCharType), Some(CIntegerLiteral(fieldTypes.length)))
    )

    val baseLength = if isTuple then 2 else 2 + tpe.classSymbol.get.name.length

    val stringLengthDecl = CVarDecl(
      "strLength",
      CIntegerType,
      Some(CIntegerLiteral(baseLength + 2 * (fieldTypes.length - 1)))
    )

    val fieldsToString: List[CStmt] = recordDecl.fields.zip(fieldTypes).zipWithIndex.flatMap {
      case ((f, t), i) =>
        val fieldToString = CAssignmentExpr(
          CArraySubscriptExpr(fieldStringsDecl.ref, i.lit),
          cascade.dispatch(_.compileToString)(
            CMemberExpr(productParam.ref, f.name.strip()),
            t
          )
        )

        val addFieldStringLength = CPlusAssignmentExpr(
          stringLengthDecl.ref,
          CCallExpr(StringH.strlen.ref, List(CArraySubscriptExpr(fieldStringsDecl.ref, i.lit)))
        )

        List(fieldToString, addFieldStringLength)
    }

    val strDecl = CompileString.stringDecl("str", stringLengthDecl.ref)

    val formatString = (if isTuple then "(" else tpe.classSymbol.get.name + "(") +
      fieldTypes.map(_ => "%s").mkString(", ") + ")"

    val sprintf = CCallExpr(
      StdIOH.sprintf.ref,
      strDecl.ref ::
        CStringLiteral(formatString) ::
        fieldTypes.indices.map(i => CArraySubscriptExpr(fieldStringsDecl.ref, i.lit)).toList
    )

    val freeFieldStrings = fieldTypes.indices.map[CStmt](
      i => CCallExpr(StdLibH.free.ref, List(CArraySubscriptExpr(fieldStringsDecl.ref, i.lit)))
    ).toList

    val body = CCompoundStmt(List(
      fieldStringsDecl,
      stringLengthDecl,
      CCompoundStmt(fieldsToString),
      strDecl,
      sprintf,
      CCompoundStmt(freeFieldStrings),
      CReturnStmt(Some(strDecl.ref))
    ))

    CFunctionDecl(
      name,
      List(productParam),
      CPointerType(CCharType),
      Some(body)
    )
  }
}
