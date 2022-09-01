package compiler.ext

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.{CAndExpr, CEqualsExpr, COrExpr, CPlusExpr}
import clangast.expr.unaryop.CNotExpr
import clangast.stmt.*
import clangast.stubs.{CJSONH, StdBoolH, StdLibH, StringH}
import clangast.types.{CCharType, CIntegerType, CPointerType, CRecordType, CType, CVoidType}
import compiler.CompilerCascade
import compiler.base.*
import compiler.base.CompileType.typeArgs
import compiler.base.CompileDataStructure.{release, retain}
import compiler.context.{RecordDeclTC, TranslationContext}
import compiler.ext.CompileSerialization.{deserialize, serialize}

import scala.quoted.*

object CompileEither extends SelectPC with ApplyPC with MatchPC with TypePC with DataStructurePC with StringPC with SerializationPC {
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
        case Apply(Apply(TypeApply(Ident("deepCopy"), _), List(either)), List()) if either.tpe <:< TypeRepr.of[Either[?, ?]] =>
          CCallExpr(
            getEitherDeepCopy(either.tpe).ref,
            List(cascade.dispatch(_.compileTermToCExpr)(either))
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
          getRecordDecl(tpe).getTypeForDecl
      }
    }

  override def compileTypeRepr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC](compileTypeReprImpl)

  override def typeName(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Either[?, ?]] =>
          val typeArgs(List(leftType, rightType)) = tpe.widen
          cascade.dispatch(_.classTypeName)(TypeRepr.of[Either].appliedTo(List(leftType, rightType)))
      }
    }

  override def compileTypeToCRecordDecl(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Either[?, ?]] =>
          val typeArgs(List(leftType, rightType)) = tpe.widen

          val leftFieldDecl = CFieldDecl(leftField, cascade.dispatch(_.compileTypeRepr)(leftType))
          val rightFieldDecl = CFieldDecl(rightField, cascade.dispatch(_.compileTypeRepr)(rightType))
          val isRightFieldDecl = CFieldDecl(isRightField, StdBoolH.bool)

          val refCountFieldDecl =
            if cascade.dispatch(_.usesRefCount)(leftType) || cascade.dispatch(_.usesRefCount)(rightType) then
              List(CFieldDecl(refCountField, CPointerType(CIntegerType)))
            else Nil

          CRecordDecl(
            "Either_" + cascade.dispatch(_.typeName)(leftType) + "_" + cascade.dispatch(_.typeName)(rightType),
            List(leftFieldDecl, rightFieldDecl, isRightFieldDecl) ++ refCountFieldDecl
          )
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

  private def compileFreeImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Either[?, ?]] =>
          val typeArgs(List(leftType, rightType)) = tpe.widen

          val releaseLeft = release(CMemberExpr(expr, leftField), leftType, CFalseLiteral).getOrElse(CNullStmt)

          val releaseRight = release(CMemberExpr(expr, rightField), rightType, CFalseLiteral).getOrElse(CNullStmt)

          CCompoundStmt(List(
            CIfStmt(
              CMemberExpr(expr, isRightField),
              releaseRight,
              Some(releaseLeft)
            ),
            CCallExpr(StdLibH.free.ref, List(CMemberExpr(expr, refCountField)))
          ))
      }
    }

  override def compileFree(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = ensureCtx[RecordDeclTC](compileFreeImpl)

  private def compileDeepCopyImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Either[?, ?]] && cascade.dispatch(_.usesRefCount)(tpe) =>
          getEitherDeepCopy(tpe)
      }
    }

  override def compileDeepCopy(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileDeepCopyImpl)

  private def compilePrintImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Either[?, ?]] =>
          CCallExpr(getEitherPrinter(tpe).ref, List(expr))
      }
    }

  override def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC](compilePrintImpl)

  def compileToStringImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Either[?, ?]] =>
          CCallExpr(getEitherToString(tpe).ref, List(expr))
      }
    }

  override def compileToString(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = ensureCtx[RecordDeclTC](compileToStringImpl)

  override def serializationRetainsEquality(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Either[?, ?]] =>
          val typeArgs(List(leftType, rightType)) = tpe.widen
          cascade.dispatch(_.serializationRetainsEquality)(leftType) && cascade.dispatch(_.serializationRetainsEquality)(rightType)
      }
    }

  private def compileSerializeImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Either[?, ?]] =>
          getEitherSerialize(tpe)
      }
    }

  override def compileSerialize(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileSerializeImpl)

  private def compileDeserializeImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Either[?, ?]] =>
          getEitherDeserialize(tpe)
      }
    }

  override def compileDeserialize(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileDeserializeImpl)

  private val leftField = "left"
  private val rightField = "right"
  private val isRightField = "isRight"

  private val CREATE_LEFT = "CREATE_LEFT"
  private val CREATE_RIGHT = "CREATE_RIGHT"
  private val EQUALS = "EQUALS"
  private val PRINT = "PRINT"
  private val TO_STRING = "TO_STRING"

  private def getLeftCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE_LEFT, buildLeftCreator(tpe))
  }

  private def getRightCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE_RIGHT, buildRightCreator(tpe))
  }

  private def buildLeftCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    val typeArgs(List(leftType, _)) = tpe.widen
    val recordDecl = getRecordDecl(tpe)
    val name = "createLeft_" + recordDecl.name

    val leftParam = CParmVarDecl("left", recordDecl.getField(leftField).declaredType)

    val eitherDecl = CVarDecl(
      "either",
      recordDecl.getTypeForDecl,
      Some(CDesignatedInitExpr(
        List(
          leftField -> retain(leftParam.ref, leftType),
          isRightField -> CFalseLiteral,
        ) ++ allocRefCount(tpe)
      ))
    )

    val body = CCompoundStmt(List(
      eitherDecl,
      CReturnStmt(Some(eitherDecl.ref))
    ))

    CFunctionDecl(name, List(leftParam), recordDecl.getTypeForDecl, Some(body))
  }

  private def buildRightCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    val typeArgs(List(_, rightType)) = tpe.widen
    val recordDecl = getRecordDecl(tpe)
    val name = "createRight_" + recordDecl.name

    val rightParam = CParmVarDecl("right", recordDecl.getField(rightField).declaredType)

    val eitherDecl = CVarDecl(
      "eith",
      recordDecl.getTypeForDecl,
      Some(CDesignatedInitExpr(
        List(
          rightField -> retain(rightParam.ref, rightType),
          isRightField -> CTrueLiteral,
        ) ++ allocRefCount(tpe)
      ))
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

    val recordDecl = getRecordDecl(tpe)
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

  private def getEitherDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> DEEP_COPY, buildEitherDeepCopy(tpe))
  }

  private def buildEitherDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(leftType, rightType)) = tpe.widen

    val name = "deepCopy_" + recordDecl.name

    val eitherParam = CParmVarDecl("either", recordDecl.getTypeForDecl)

    val copyLeft =
      CCallExpr(
        getLeftCreator(tpe).ref,
        List(CompileDataStructure.deepCopy(
          CMemberExpr(eitherParam.ref, leftField),
          leftType
        ))
      )

    val copyRight =
      CCallExpr(
        getRightCreator(tpe).ref,
        List(CompileDataStructure.deepCopy(
          CMemberExpr(eitherParam.ref, rightField),
          rightType
        ))
      )

    val body = CCompoundStmt(List(
      CIfStmt(
        CMemberExpr(eitherParam.ref, isRightField),
        CReturnStmt(Some(copyRight)),
        Some(CReturnStmt(Some(copyLeft)))
      )
    ))

    CFunctionDecl(name, List(eitherParam), recordDecl.getTypeForDecl, Some(body))
  }

  private def getEitherPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> PRINT, buildEitherPrinter(tpe))
  }

  private def buildEitherPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
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

  private def getEitherToString(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> TO_STRING, buildEitherToString(tpe))
  }

  private def buildEitherToString(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(leftType, rightType)) = tpe.widen

    val name = "toString_" + recordDecl.name

    val eitherParam = CParmVarDecl("either", recordDecl.getTypeForDecl)

    val rightValStringDecl = CVarDecl(
      "rightString",
      CPointerType(CCharType),
      Some(cascade.dispatch(_.compileToString)(CMemberExpr(eitherParam.ref, rightField), rightType))
    )

    val rightStringDecl = CompileString.stringDecl(
      "str",
      CPlusExpr(7.lit, CCallExpr(StringH.strlen.ref, List(rightValStringDecl.ref)))
    )

    val rightSprintf = CompileString.sprintf(rightStringDecl.ref, "Right(%s)", rightValStringDecl.ref)

    val freeRight = CCallExpr(StdLibH.free.ref, List(rightValStringDecl.ref))

    val rightBranch = CCompoundStmt(List(
      rightValStringDecl,
      rightStringDecl,
      rightSprintf,
      freeRight,
      CReturnStmt(Some(rightStringDecl.ref))
    ))

    val leftValStringDecl = CVarDecl(
      "leftString",
      CPointerType(CCharType),
      Some(cascade.dispatch(_.compileToString)(CMemberExpr(eitherParam.ref, leftField), leftType))
    )

    val leftStringDecl = CompileString.stringDecl(
      "str",
      CPlusExpr(6.lit, CCallExpr(StringH.strlen.ref, List(leftValStringDecl.ref)))
    )

    val leftSprintf = CompileString.sprintf(leftStringDecl.ref, "Left(%s)", leftValStringDecl.ref)

    val freeLeft = CCallExpr(StdLibH.free.ref, List(leftValStringDecl.ref))

    val leftBranch = CCompoundStmt(List(
      leftValStringDecl,
      leftStringDecl,
      leftSprintf,
      freeLeft,
      CReturnStmt(Some(leftStringDecl.ref))
    ))

    val body = CCompoundStmt(List(
      CIfStmt(
        CMemberExpr(eitherParam.ref, isRightField),
        rightBranch,
        Some(leftBranch)
      )
    ))

    CFunctionDecl(name, List(eitherParam), CPointerType(CCharType), Some(body))
  }

  private def getEitherSerialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> SERIALIZE, buildEitherSerialize(tpe))
  }

  private def buildEitherSerialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(leftType, rightType)) = tpe.widen

    val name = "serialize_" + recordDecl.name

    val eitherParam = CParmVarDecl("either", recordDecl.getTypeForDecl)

    val jsonDecl = CVarDecl("json", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_CreateObject.ref, List())))

    val leftBranch = CCompoundStmt(List(
      CCallExpr(CJSONH.cJSON_AddStringToObject.ref, List(jsonDecl.ref, CStringLiteral("type"), CStringLiteral("Left"))),
      CCallExpr(
        CJSONH.cJSON_AddItemToObject.ref,
        List(jsonDecl.ref, CStringLiteral("value"), serialize(CMemberExpr(eitherParam.ref, leftField), leftType))
      )
    ))

    val rightBranch = CCompoundStmt(List(
      CCallExpr(CJSONH.cJSON_AddStringToObject.ref, List(jsonDecl.ref, CStringLiteral("type"), CStringLiteral("Right"))),
      CCallExpr(
        CJSONH.cJSON_AddItemToObject.ref,
        List(jsonDecl.ref, CStringLiteral("value"), serialize(CMemberExpr(eitherParam.ref, rightField), rightType))
      )
    ))

    val body = CCompoundStmt(List(
      jsonDecl,
      CIfStmt(
        CMemberExpr(eitherParam.ref, isRightField),
        rightBranch,
        Some(leftBranch)
      ),
      CReturnStmt(Some(jsonDecl.ref))
    ))

    CFunctionDecl(name, List(eitherParam), CPointerType(CJSONH.cJSON), Some(body))
  }

  private def getEitherDeserialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> DESERIALIZE, buildEitherDeserialize(tpe))
  }

  private def buildEitherDeserialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(leftType, rightType)) = tpe.widen

    val name = "deserialize_" + recordDecl.name

    val jsonParam = CParmVarDecl("json", CPointerType(CJSONH.cJSON))

    val leftBranch = CReturnStmt(Some(
      CCallExpr(
        getLeftCreator(tpe).ref,
        List(deserialize(CCallExpr(CJSONH.cJSON_GetObjectItem.ref, List(jsonParam.ref, CStringLiteral("value"))), leftType))
      )
    ))

    val rightBranch = CReturnStmt(Some(
      CCallExpr(
        getRightCreator(tpe).ref,
        List(deserialize(CCallExpr(CJSONH.cJSON_GetObjectItem.ref, List(jsonParam.ref, CStringLiteral("value"))), rightType))
      )
    ))

    val body = CCompoundStmt(List(
      CIfStmt(
        CEqualsExpr(
          CCallExpr(
            StringH.strcmp.ref,
            List(
              CJSONH.valuestring(CCallExpr(CJSONH.cJSON_GetObjectItem.ref, List(jsonParam.ref, CStringLiteral("type")))),
              CStringLiteral("Right")
            )
          ),
          0.lit
        ),
        rightBranch,
        Some(leftBranch)
      )
    ))

    CFunctionDecl(name, List(jsonParam), recordDecl.getTypeForDecl, Some(body))
  }
}
