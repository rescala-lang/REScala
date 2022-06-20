package compiler.ext

import clangast.given
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.{CAndExpr, COrExpr}
import clangast.expr.unaryop.CNotExpr
import clangast.stmt.*
import clangast.stubs.StdBoolH
import clangast.types.{CRecordType, CType, CVoidType}
import compiler.CompilerCascade
import compiler.base.*
import compiler.base.CompileType.typeArgs
import compiler.base.CompileDataStructure.{retain, release}
import compiler.context.{RecordDeclTC, TranslationContext}

import scala.quoted.*

object CompileEither extends SelectPC with ApplyPC with MatchPC with TypePC with DataStructurePC with StringPC {
  private def compileSelectImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = {
      import quotes.reflect.*

      {
        case Select(either, "isLeft") if either.tpe <:< TypeRepr.of[Either[?, ?]] =>
          CNotExpr(CMemberExpr(cascade.dispatch(_.compileTermToCExpr)(either), isRightField))
        case Select(either, "isRight") if either.tpe <:< TypeRepr.of[Either[?, ?]] =>
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
            getLeftCreator(apply.tpe).ref,
            List(cascade.dispatch(_.compileTermToCExpr)(inner))
          )
        case apply @ Apply(TypeApply(Select(Ident("Right"), "apply"), _), List(inner)) =>
          CCallExpr(
            getRightCreator(apply.tpe).ref,
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
            getEitherEquals(leftType).ref,
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
          val subPrefix = CMemberExpr(prefix, leftField)
          val typeArgs(List(leftType, _)) = prefixType.widen

          val isLeftCond = CNotExpr(CMemberExpr(prefix, isRightField))

          val (subCond, subDecls) = cascade.dispatch(_.compilePattern)(subPattern, subPrefix, leftType)

          val combinedCond = CompileMatch.combineCond(Some(isLeftCond), subCond)

          (combinedCond, subDecls)
        case (TypedOrTest(Unapply(TypeApply(Select(Ident("Right"), "unapply"), _), _, List(subPattern)), _), prefix, prefixType) =>
          val subPrefix = CMemberExpr(prefix, rightField)
          val typeArgs(List(_, rightType)) = prefixType.widen

          val isRightCond = CMemberExpr(prefix, isRightField)

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
          getEitherRecordDecl(tpe).getTypeForDecl
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

  override def usesRefCount(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Either[?, ?]] =>
          val typeArgs(List(leftType, rightType)) = tpe.widen
          cascade.dispatch(_.usesRefCount)(leftType) || cascade.dispatch(_.usesRefCount)(rightType)
      }
    }

  private def compileRetainImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Either[?, ?]] && usesRefCount(tpe) => getEitherRetain(tpe)
      }
    }

  override def compileRetain(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileRetainImpl)

  private def compileReleaseImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Either[?, ?]] && usesRefCount(tpe) => getEitherRelease(tpe)
      }
    }

  override def compileRelease(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileReleaseImpl)

  def compilePrintImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = {
      import quotes.reflect.*
  
      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Either[?, ?]] =>
          CCallExpr(getEitherPrinter(tpe).ref, List(expr))
      }
    }

  override def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC](compilePrintImpl)

  private def getEitherRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CRecordDecl = {
    ctx.nameToRecordDecl.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe), compileEitherTypeToCRecordDecl(tpe))
  }

  private val leftField = "left"
  private val rightField = "right"
  private val isRightField = "isRight"
  
  private def compileEitherTypeToCRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): CRecordDecl = {
    import quotes.reflect.*

    val typeArgs(List(leftType, rightType)) = tpe

    val leftFieldDecl = CFieldDecl(leftField, cascade.dispatch(_.compileTypeRepr)(leftType))
    val rightFieldDecl = CFieldDecl(rightField, cascade.dispatch(_.compileTypeRepr)(rightType))
    val isRightFieldDecl = CFieldDecl(isRightField, StdBoolH.bool)

    CRecordDecl(
      "Either_" + cascade.dispatch(_.typeName)(leftType) + "_" + cascade.dispatch(_.typeName)(rightType),
      List(leftFieldDecl, rightFieldDecl, isRightFieldDecl)
    )
  }

  private val CREATE_LEFT = "CREATE_LEFT"
  private val CREATE_RIGHT = "CREATE_RIGHT"
  private val EQUALS = "EQUALS"
  private val PRINT = "PRINT"
  private val RETAIN = "RETAIN"
  private val RELEASE = "RELEASE"

  private def getLeftCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE_LEFT, buildLeftCreator(getEitherRecordDecl(tpe)))
  }

  private def getRightCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE_RIGHT, buildRightCreator(getEitherRecordDecl(tpe)))
  }

  private def buildLeftCreator(recordDecl: CRecordDecl)(using ctx: TranslationContext): CFunctionDecl = {
    val name = "createLeft_" + recordDecl.name

    val leftParam = CParmVarDecl("left", recordDecl.getField(leftField).declaredType)

    val eitherDecl = CVarDecl(
      "either",
      recordDecl.getTypeForDecl,
      Some(CDesignatedInitExpr(List(
        leftField -> leftParam.ref,
        isRightField -> CFalseLiteral
      )))
    )

    val body = CCompoundStmt(List(
      eitherDecl,
      CReturnStmt(Some(eitherDecl.ref))
    ))

    CFunctionDecl(name, List(leftParam), recordDecl.getTypeForDecl, Some(body))
  }

  private def buildRightCreator(recordDecl: CRecordDecl)(using ctx: TranslationContext): CFunctionDecl = {
    val name = "createRight_" + recordDecl.name

    val rightParam = CParmVarDecl("right", recordDecl.getField(rightField).declaredType)

    val eitherDecl = CVarDecl(
      "eith",
      recordDecl.getTypeForDecl,
      Some(CDesignatedInitExpr(List(
        rightField -> rightParam.ref,
        isRightField -> CTrueLiteral
      )))
    )

    val body = CCompoundStmt(List(
      eitherDecl,
      CReturnStmt(Some(eitherDecl.ref))
    ))

    CFunctionDecl(name, List(rightParam), recordDecl.getTypeForDecl, Some(body))
  }

  private def getEitherEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> EQUALS, buildEitherEquals(tpe))
  }

  private def buildEitherEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getEitherRecordDecl(tpe)
    val typeArgs(List(leftType, rightType)) = tpe.widen

    val name = "equals_" + recordDecl.name

    val paramLeft = CParmVarDecl("left", recordDecl.getTypeForDecl)
    val paramRight = CParmVarDecl("right", recordDecl.getTypeForDecl)

    val equalsExpr = COrExpr(
      CAndExpr(
        CAndExpr(
          CMemberExpr(paramLeft.ref, isRightField),
          CMemberExpr(paramRight.ref, isRightField)
        ),
        cascade.dispatch(_.compileEquals)(
          CMemberExpr(paramLeft.ref, rightField),
          rightType,
          CMemberExpr(paramRight.ref, rightField),
          rightType,
        )
      ),
      CAndExpr(
        CAndExpr(
          CNotExpr(CMemberExpr(paramLeft.ref, isRightField)),
          CNotExpr(CMemberExpr(paramRight.ref, isRightField))
        ),
        cascade.dispatch(_.compileEquals)(
          CMemberExpr(paramLeft.ref, leftField),
          leftType,
          CMemberExpr(paramRight.ref, leftField),
          leftType,
        )
      )
    )

    val body = CCompoundStmt(List(CReturnStmt(Some(equalsExpr))))

    CFunctionDecl(name, List(paramLeft, paramRight), StdBoolH.bool, Some(body))
  }

  private def getEitherRetain(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> RETAIN, buildEitherRetain(tpe))
  }

  private def buildEitherRetain(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getEitherRecordDecl(tpe)
    val typeArgs(List(leftType, rightType)) = tpe.widen

    val name = "retain_" + recordDecl.name

    val eitherParam = CParmVarDecl("either", recordDecl.getTypeForDecl)

    val retainLeft: CStmt = if cascade.dispatch(_.usesRefCount)(leftType) then
      retain(CMemberExpr(eitherParam.ref, leftField), leftType)
    else CNullStmt

    val retainRight: CStmt = if cascade.dispatch(_.usesRefCount)(rightType) then
      retain(CMemberExpr(eitherParam.ref, rightField), rightType)
    else CNullStmt

    val body = CCompoundStmt(List(
      CIfStmt(
        CMemberExpr(eitherParam.ref, isRightField),
        retainRight,
        Some(retainLeft)
      ),
      CReturnStmt(Some(eitherParam.ref))
    ))

    CFunctionDecl(name, List(eitherParam), recordDecl.getTypeForDecl, Some(body))
  }

  private def getEitherRelease(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> RELEASE, buildEitherRelease(tpe))
  }

  private def buildEitherRelease(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getEitherRecordDecl(tpe)
    val typeArgs(List(leftType, rightType)) = tpe.widen

    val name = "release_" + recordDecl.name

    val eitherParam = CParmVarDecl("either", recordDecl.getTypeForDecl)
    val keepWithZero = CParmVarDecl("keep_with_zero", StdBoolH.bool)

    val releaseLeft = release(CMemberExpr(eitherParam.ref, leftField), leftType, keepWithZero.ref).getOrElse(CNullStmt)

    val releaseRight = release(CMemberExpr(eitherParam.ref, rightField), rightType, keepWithZero.ref).getOrElse(CNullStmt)

    val body = CCompoundStmt(List(
      CIfStmt(
        CMemberExpr(eitherParam.ref, isRightField),
        releaseRight,
        Some(releaseLeft)
      )
    ))

    CFunctionDecl(name, List(eitherParam, keepWithZero), recordDecl.getTypeForDecl, Some(body))
  }

  private def getEitherPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> PRINT, buildEitherPrinter(tpe))
  }

  private def buildEitherPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getEitherRecordDecl(tpe)
    val typeArgs(List(leftType, rightType)) = tpe.widen

    val name = "print_" + recordDecl.name

    val eitherParam = CParmVarDecl("either", recordDecl.getTypeForDecl)

    val body = CCompoundStmt(List(
      CIfStmt(
        CMemberExpr(eitherParam.ref, isRightField),
        CCompoundStmt(List(
          CompileString.printf("Right("),
          cascade.dispatch(_.compilePrint)(
            CMemberExpr(eitherParam.ref, rightField),
            rightType
          ),
        )),
        Some(CCompoundStmt(List(
          CompileString.printf("Left("),
          cascade.dispatch(_.compilePrint)(
            CMemberExpr(eitherParam.ref, leftField),
            leftType
          ),
        )))
      ),
      CompileString.printf(")")
    ))

    CFunctionDecl(name, List(eitherParam), CVoidType, Some(body))
  }
}
