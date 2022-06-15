package compiler.ext

import clangast.given
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.{CAndExpr, COrExpr}
import clangast.expr.unaryop.CNotExpr
import clangast.stmt.{CCompoundStmt, CIfStmt, CReturnStmt, CStmt}
import clangast.stubs.StdBoolH
import clangast.types.{CRecordType, CType, CVoidType}
import compiler.CompilerCascade
import compiler.base.*
import compiler.base.CompileType.typeArgs
import compiler.context.{RecordDeclTC, TranslationContext}

import scala.quoted.*

object CompileEither extends SelectPC with ApplyPC with MatchPC with TypePC with StringPC {
  private def compileSelectImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = {
      import quotes.reflect.*

      {
        case Select(either, "isLeft") if either.tpe <:< TypeRepr.of[Either[?, ?]] =>
          val isRightField = getIsRightField(getEitherRecordDecl(either.tpe))
          CNotExpr(CMemberExpr(cascade.dispatch(_.compileTermToCExpr)(either), isRightField))
        case Select(either, "isRight") if either.tpe <:< TypeRepr.of[Either[?, ?]] =>
          val isRightField = getIsRightField(getEitherRecordDecl(either.tpe))
          CMemberExpr(cascade.dispatch(_.compileTermToCExpr)(either), isRightField)
      }
    }

  override def compileSelect(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = ensureCtx[RecordDeclTC](compileSelectImpl)

  private def compileApplyImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*

      {
        case apply @ Apply(TypeApply(Select(Ident("Left"), "apply"), _), List(inner)) =>
          CCallExpr(
            CDeclRefExpr(getLeftCreator(apply.tpe)),
            List(cascade.dispatch(_.compileTermToCExpr)(inner))
          )
        case apply @ Apply(TypeApply(Select(Ident("Right"), "apply"), _), List(inner)) =>
          CCallExpr(
            CDeclRefExpr(getRightCreator(apply.tpe)),
            List(cascade.dispatch(_.compileTermToCExpr)(inner))
          )
      }
    }

  override def compileApply(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[RecordDeclTC](compileApplyImpl)

  private def compileEqualsImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] = {
      import quotes.reflect.*

      {
        case (leftExpr, leftType, rightExpr, _) if leftType <:< TypeRepr.of[Either[?, ?]] =>
          CCallExpr(
            CDeclRefExpr(getEitherEquals(leftType)),
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
        case (TypedOrTest(Unapply(TypeApply(Select(Ident("Left"), "unapply"), _), _, List(subPattern)), _), prefix, prefixType) =>
          val recordDecl = getEitherRecordDecl(prefixType)
          val subPrefix = CMemberExpr(prefix, getLeftField(recordDecl))
          val typeArgs(List(leftType, _)) = prefixType.widen

          val isLeftCond = CNotExpr(CMemberExpr(prefix, getIsRightField(recordDecl)))

          val (subCond, subDecls) = cascade.dispatch(_.compilePattern)(subPattern, subPrefix, leftType)

          val combinedCond = CompileMatch.combineCond(Some(isLeftCond), subCond)

          (combinedCond, subDecls)
        case (TypedOrTest(Unapply(TypeApply(Select(Ident("Right"), "unapply"), _), _, List(subPattern)), _), prefix, prefixType) =>
          val recordDecl = getEitherRecordDecl(prefixType)
          val subPrefix = CMemberExpr(prefix, getRightField(recordDecl))
          val typeArgs(List(_, rightType)) = prefixType.widen

          val isRightCond = CMemberExpr(prefix, getIsRightField(recordDecl))

          val (subCond, subDecls) = cascade.dispatch(_.compilePattern)(subPattern, subPrefix, rightType)

          val combinedCond = CompileMatch.combineCond(Some(isRightCond), subCond)

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
        case tpe if tpe <:< TypeRepr.of[Either[?, ?]] =>
          CRecordType(getEitherRecordDecl(tpe))
      }
    }

  override def compileTypeRepr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC](compileTypeReprImpl)

  override def typeName(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Either[?, ?]] => cascade.dispatch(_.classTypeName)(tpe)
      }
    }

  def compilePrintImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = {
      import quotes.reflect.*
  
      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Either[?, ?]] =>
          CCallExpr(CDeclRefExpr(getEitherPrinter(tpe)), List(expr))
      }
    }

  override def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC](compilePrintImpl)

  private def getEitherRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CRecordDecl = {
    ctx.nameToRecordDecl.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe), compileEitherTypeToCRecordDecl(tpe))
  }

  private def compileEitherTypeToCRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): CRecordDecl = {
    import quotes.reflect.*

    val typeArgs(List(leftType, rightType)) = tpe

    val leftField = CFieldDecl("left", cascade.dispatch(_.compileTypeRepr)(leftType))
    val rightField = CFieldDecl("right", cascade.dispatch(_.compileTypeRepr)(rightType))
    val isRightField = CFieldDecl("isRight", StdBoolH.bool)

    CRecordDecl(
      "Either_" + cascade.dispatch(_.typeName)(leftType) + "_" + cascade.dispatch(_.typeName)(rightType),
      List(leftField, rightField, isRightField)
    )
  }

  private def getLeftField(recordDecl: CRecordDecl): CFieldDecl = recordDecl.getField("left")
  private def getRightField(recordDecl: CRecordDecl): CFieldDecl = recordDecl.getField("right")
  private def getIsRightField(recordDecl: CRecordDecl): CFieldDecl = recordDecl.getField("isRight")

  private val CREATE_LEFT = "CREATE_LEFT"
  private val CREATE_RIGHT = "CREATE_RIGHT"
  private val EQUALS = "EQUALS"
  private val PRINT = "PRINT"

  private def getLeftCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE_LEFT, buildLeftCreator(getEitherRecordDecl(tpe)))
  }

  private def getRightCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE_RIGHT, buildRightCreator(getEitherRecordDecl(tpe)))
  }

  private def buildLeftCreator(recordDecl: CRecordDecl)(using ctx: TranslationContext): CFunctionDecl = {
    val name = "createLeft_" + recordDecl.name

    val leftParam = CParmVarDecl("left", getLeftField(recordDecl).declaredType)

    val eitherDecl = CVarDecl(
      "eith",
      recordDecl.getTypeForDecl,
      Some(CDesignatedInitExpr(List(
        "left" -> CDeclRefExpr(leftParam),
        "isRight" -> CFalseLiteral
      )))
    )

    val body = CCompoundStmt(List(
      eitherDecl,
      CReturnStmt(Some(CDeclRefExpr(eitherDecl)))
    ))

    CFunctionDecl(name, List(leftParam), recordDecl.getTypeForDecl, Some(body))
  }

  private def buildRightCreator(recordDecl: CRecordDecl)(using ctx: TranslationContext): CFunctionDecl = {
    val name = "createRight_" + recordDecl.name

    val rightParam = CParmVarDecl("right", getRightField(recordDecl).declaredType)

    val eitherDecl = CVarDecl(
      "eith",
      recordDecl.getTypeForDecl,
      Some(CDesignatedInitExpr(List(
        "right" -> CDeclRefExpr(rightParam),
        "isRight" -> CTrueLiteral
      )))
    )

    val body = CCompoundStmt(List(
      eitherDecl,
      CReturnStmt(Some(CDeclRefExpr(eitherDecl)))
    ))

    CFunctionDecl(name, List(rightParam), recordDecl.getTypeForDecl, Some(body))
  }

  private def getEitherEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> EQUALS, buildEitherEquals(tpe))
  }

  private def buildEitherEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getEitherRecordDecl(tpe)
    val leftField = getLeftField(recordDecl)
    val rightField = getRightField(recordDecl)
    val isRightField = getIsRightField(recordDecl)
    val typeArgs(List(leftType, rightType)) = tpe.widen

    val name = "equals_" + recordDecl.name

    val paramLeft = CParmVarDecl("left", recordDecl.getTypeForDecl)
    val paramRight = CParmVarDecl("right", recordDecl.getTypeForDecl)

    val equalsExpr = COrExpr(
      CAndExpr(
        CAndExpr(
          CMemberExpr(CDeclRefExpr(paramLeft), isRightField),
          CMemberExpr(CDeclRefExpr(paramRight), isRightField)
        ),
        cascade.dispatch(_.compileEquals)(
          CMemberExpr(CDeclRefExpr(paramLeft), rightField),
          rightType,
          CMemberExpr(CDeclRefExpr(paramRight), rightField),
          rightType,
        )
      ),
      CAndExpr(
        CAndExpr(
          CNotExpr(CMemberExpr(CDeclRefExpr(paramLeft), isRightField)),
          CNotExpr(CMemberExpr(CDeclRefExpr(paramRight), isRightField))
        ),
        cascade.dispatch(_.compileEquals)(
          CMemberExpr(CDeclRefExpr(paramLeft), leftField),
          leftType,
          CMemberExpr(CDeclRefExpr(paramRight), leftField),
          leftType,
        )
      )
    )

    val body = CCompoundStmt(List(CReturnStmt(Some(equalsExpr))))

    CFunctionDecl(name, List(paramLeft, paramRight), StdBoolH.bool, Some(body))
  }

  private def getEitherPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> PRINT, buildEitherPrinter(tpe))
  }

  private def buildEitherPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getEitherRecordDecl(tpe)
    val leftField = getLeftField(recordDecl)
    val rightField = getRightField(recordDecl)
    val isRightField = getIsRightField(recordDecl)
    val typeArgs(List(leftType, rightType)) = tpe.widen

    val name = "print_" + recordDecl.name

    val eitherParam = CParmVarDecl("either", recordDecl.getTypeForDecl)

    val body = CCompoundStmt(List(
      CIfStmt(
        CMemberExpr(CDeclRefExpr(eitherParam), isRightField),
        CCompoundStmt(List(
          CompileString.printf("Right("),
          cascade.dispatch(_.compilePrint)(
            CMemberExpr(CDeclRefExpr(eitherParam), rightField),
            rightType
          ),
        )),
        Some(CCompoundStmt(List(
          CompileString.printf("Left("),
          cascade.dispatch(_.compilePrint)(
            CMemberExpr(CDeclRefExpr(eitherParam), leftField),
            leftType
          ),
        )))
      ),
      CompileString.printf(")")
    ))

    CFunctionDecl(name, List(eitherParam), CVoidType, Some(body))
  }
}
