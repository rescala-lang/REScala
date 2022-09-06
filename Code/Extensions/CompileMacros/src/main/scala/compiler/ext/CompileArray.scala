package compiler.ext

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.binaryop.*
import clangast.expr.unaryop.*
import clangast.expr.*
import clangast.stmt.*
import clangast.stubs.{CJSONH, StdArgH, StdBoolH, StdLibH, StringH}
import clangast.types.*
import compiler.CompilerCascade
import compiler.base.*
import compiler.base.CompileType.typeArgs
import compiler.base.CompileApply.varArgs
import compiler.base.CompileDataStructure.{release, retain}
import compiler.context.{FunctionDeclTC, RecordDeclTC, TranslationContext}
import compiler.ext.CompileSerialization.{deserialize, serialize}

import scala.quoted.*

object CompileArray extends SelectPC with ApplyPC with MatchPC with TypePC with DataStructurePC with StringPC with SerializationPC {
  private def compileSelectImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = {
      import quotes.reflect.*

      {
        case Select(arr, "length") =>
          CMemberExpr(
            cascade.dispatch(_.compileTermToCExpr)(arr),
            lengthField
          )
      }
    }

  override def compileSelect(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = ensureCtx[RecordDeclTC](compileSelectImpl)

  private def compileApplyImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*

      {
        case apply @ Apply(Apply(TypeApply(Select(Ident("Array"), "fill"), _), List(n)), List(elem)) =>
          CCallExpr(
            getArrayFill(apply.tpe).ref,
            List(
              cascade.dispatch(_.compileTermToCExpr)(n),
              cascade.dispatch(_.compileTermToCExpr)(elem)
            )
          )
        case apply @ Apply(TypeApply(Select(Ident("Array"), "ofDim"), List(tpt)), List(n)) if CompileType.hasDefaultValue(tpt.tpe) =>
          CCallExpr(
            getArrayFill(apply.tpe).ref,
            List(
              cascade.dispatch(_.compileTermToCExpr)(n),
              cascade.dispatch(_.defaultValue)(tpt.tpe)
            )
          )
        case apply @ this.arrayApply(args) =>
          val elems = args.map(cascade.dispatch(_.compileTermToCExpr))

          CCallExpr(getArrayCreator(apply.tpe).ref, CIntegerLiteral(elems.length) :: elems)
        case Apply(Select(arr, "apply"), List(idx)) if arr.tpe <:< TypeRepr.of[Array[?]] =>
          arrayIndexAccess(arr, idx)
        case Apply(Select(arr, "update"), List(idx, v)) if arr.tpe <:< TypeRepr.of[Array[?]] =>
          arrayIndexUpdate(arr, idx, v)
        case Apply(Apply(TypeApply(Ident("deepCopy"), _), List(arr)), List()) if arr.tpe <:< TypeRepr.of[Array[?]] =>
          CCallExpr(
            getArrayDeepCopy(arr.tpe).ref,
            List(cascade.dispatch(_.compileTermToCExpr)(arr))
          )
      }
    }

  override def compileApply(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[RecordDeclTC](compileApplyImpl)

  private def compilePatternImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
  PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] = {
    import quotes.reflect.*

    {
      case (Unapply(TypeApply(Select(Ident("Array"), "unapplySeq"), _), _, subPatterns), prefix, prefixType) =>
        val typeArgs(List(elemType)) = prefixType.widen: @unchecked

        val lengthCond = CEqualsExpr(CMemberExpr(prefix, lengthField), CIntegerLiteral(subPatterns.length))

        subPatterns.zipWithIndex.foldLeft((Option[CExpr](lengthCond), List.empty[CVarDecl])) {
          case ((cond, decls), (subPattern, i)) =>
            val (subCond, subDecls) = cascade.dispatch(_.compilePattern)(
              subPattern,
              CArraySubscriptExpr(CMemberExpr(prefix, dataField), i.lit),
              elemType
            )

            val combinedCond = CompileMatch.combineCond(cond, subCond)

            (combinedCond, subDecls ++ decls)
        }
    }
  }

  override def compilePattern(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] =
      ensureCtx[RecordDeclTC](compilePatternImpl)

  private def compileEqualsImpl(using Quotes)(using RecordDeclTC, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] = {
      import quotes.reflect.*

      {
        case (leftExpr, leftType, rightExpr, _) if leftType <:< TypeRepr.of[Array[?]] =>
          CParenExpr(CEqualsExpr(
            CMemberExpr(leftExpr, dataField),
            CMemberExpr(rightExpr, dataField)
          ))
      }
    }

  override def compileEquals(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] =
      ensureCtx[RecordDeclTC](compileEqualsImpl)

  private def compileTypeReprImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] =>
          getRecordDecl(tpe).getTypeForDecl
      }
    }

  override def compileTypeRepr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC](compileTypeReprImpl)

  override def typeName(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] => cascade.dispatch(_.classTypeName)(tpe)
      }
    }

  private def defaultValueImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CExpr] = {
      import quotes.reflect.*
  
      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] =>
          CCallExpr(getArrayCreator(tpe).ref, List())
      }
    }

  override def defaultValue(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CExpr] = ensureCtx[RecordDeclTC](defaultValueImpl)

  override def compileTypeToCRecordDecl(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] =>
          val typeArgs(List(elemType)) = tpe.widen: @unchecked

          val dataFieldDecl = CFieldDecl(dataField, CPointerType(cascade.dispatch(_.compileTypeRepr)(elemType)))
          val lengthFieldDecl = CFieldDecl(lengthField, CIntegerType)
          val refCountFieldDecl = CFieldDecl(refCountField, CPointerType(CIntegerType))

          CRecordDecl("Array_" + cascade.dispatch(_.typeName)(elemType), List(dataFieldDecl, lengthFieldDecl, refCountFieldDecl))
      }
    }

  override def usesRefCount(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] => true
      }
    }

  private def compileFreeImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Array[?]] =>
          val typeArgs(List(elemType)) = tpe.widen: @unchecked

          val propagateRelease = cascade.dispatch(_.usesRefCount)(elemType)

          val freeThis: List[CStmt] = List(
            CCallExpr(StdLibH.free.ref, List(CMemberExpr(expr, refCountField))),
            CCallExpr(StdLibH.free.ref, List(CMemberExpr(expr, dataField)))
          )

          if propagateRelease then
            val iter = CVarDecl("i", CIntegerType, Some(0.lit))

            val loop = CForStmt(
              Some(iter),
              Some(CLessThanExpr(iter.ref, CMemberExpr(expr, lengthField))),
              Some(CIncExpr(iter.ref)),
              CCompoundStmt(List(
                release(
                  CArraySubscriptExpr(
                    CMemberExpr(expr, dataField),
                    iter.ref
                  ),
                  elemType,
                  CFalseLiteral
                ).get
              ))
            )

            CCompoundStmt(loop :: freeThis)
          else CCompoundStmt(freeThis)
      }
    }

  override def compileFree(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = ensureCtx[RecordDeclTC](compileFreeImpl)

  private def compileDeepCopyImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] =>
          getArrayDeepCopy(tpe)
      }
    }

  override def compileDeepCopy(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileDeepCopyImpl)

  private def compilePrintImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Array[?]] =>
          CCallExpr(getArrayPrinter(tpe).ref, List(expr))
      }
    }

  override def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC](compilePrintImpl)

  def compileToStringImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Array[?]] =>
          CCallExpr(getArrayToString(tpe).ref, List(expr))
      }
  }

  override def compileToString(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = ensureCtx[RecordDeclTC](compileToStringImpl)

  override def serializationRetainsEquality(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
      import quotes.reflect.*
  
      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] =>
          val typeArgs(List(elemType)) = tpe.widen: @unchecked
          cascade.dispatch(_.serializationRetainsEquality)(elemType)
      }
    }
  
  private def compileSerializeImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] =>
          getArraySerialize(tpe)
      }
    }

  override def compileSerialize(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileSerializeImpl)

  private def compileDeserializeImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Array[?]] =>
          getArrayDeserialize(tpe)
      }
    }

  override def compileDeserialize(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileDeserializeImpl)

  private val dataField: String = "data"
  private val lengthField: String = "length"

  private def arrayApply(using Quotes): PartialFunction[quotes.reflect.Apply, List[quotes.reflect.Term]] = apply => {
    import quotes.reflect.*

    apply match {
      case Apply(Select(Ident("Array"), "apply"), varArgs(args)) => args
      case Apply(TypeApply(Select(Ident("Array"), "apply"), _), varArgs(args)) => args
    }
  }

  private val CREATE = "CREATE"
  private val FILL = "FILL"
  private val PRINT = "PRINT"
  private val TO_STRING = "TO_STRING"

  private def arrayStructInitializer(using Quotes)(tpe: quotes.reflect.TypeRepr, sizeExpr: CExpr)
                                    (using ctx: RecordDeclTC, cascade: CompilerCascade): CExpr = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val CFieldDecl(_, CQualType(CPointerType(elemCType), _)) = recordDecl.getField(dataField): @unchecked

    CDesignatedInitExpr(List(
      dataField -> CCastExpr(
        CCallExpr(
          StdLibH.calloc.ref,
          List(
            sizeExpr,
            CSizeofExpr(Left(elemCType.unqualType))
          )
        ),
        CPointerType(elemCType)
      ),
      lengthField -> sizeExpr,
      allocRefCount(tpe).get
    ))
  }

  private def getArrayCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE, buildArrayCreator(tpe))
  }

  private def buildArrayCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(elemType)) = tpe.widen: @unchecked

    val name = "create_" + recordDecl.name

    val lengthParam = CParmVarDecl("length", CIntegerType)

    val CFieldDecl(_, CQualType(CPointerType(elemCType), _)) = recordDecl.getField(dataField): @unchecked

    val arrDecl =
      CVarDecl(
        "arr",
        recordDecl.getTypeForDecl,
        Some(arrayStructInitializer(tpe, lengthParam.ref))
      )

    val argpDecl = CVarDecl("argp", StdArgH.va_list.getTypeForDecl)

    val iDecl = CVarDecl("i", CIntegerType, Some(0.lit))

    val loop =
      CForStmt(
        Some(iDecl),
        Some(CLessThanExpr(iDecl.ref, lengthParam.ref)),
        Some(CIncExpr(iDecl.ref)),
        CCompoundStmt(List(
          CAssignmentExpr(
            CArraySubscriptExpr(
              CMemberExpr(arrDecl.ref, dataField),
              iDecl.ref
            ),
            retain(
              CCallExpr(
                StdArgH.va_arg.ref,
                List(argpDecl.ref, CTypeArgExpr(elemCType.unqualType))
              ),
              elemType
            )
          )
        ))
      )

    val body = CCompoundStmt(List(
      arrDecl,
      argpDecl,
      CCallExpr(StdArgH.va_start.ref, List(argpDecl.ref, lengthParam.ref)),
      loop,
      CCallExpr(StdArgH.va_end.ref, List(argpDecl.ref)),
      CReturnStmt(Some(arrDecl.ref))
    ))

    CFunctionDecl(name, List(lengthParam), recordDecl.getTypeForDecl, Some(body), variadic = true)
  }

  private def arrayIndexAccess(using Quotes)(arr: quotes.reflect.Term, idx: quotes.reflect.Term)(using ctx: TranslationContext, cascade: CompilerCascade): CArraySubscriptExpr = {
    CArraySubscriptExpr(
      CMemberExpr(
        cascade.dispatch(_.compileTermToCExpr)(arr),
        dataField
      ),
      cascade.dispatch(_.compileTermToCExpr)(idx)
    )
  }

  private def arrayIndexUpdate(using Quotes)(arr: quotes.reflect.Term, idx: quotes.reflect.Term, v: quotes.reflect.Term)
                              (using ctx: TranslationContext, cascade: CompilerCascade): CExpr = {
    val tempDecl = CVarDecl(
      "temp",
      cascade.dispatch(_.compileTypeRepr)(v.tpe),
      Some(arrayIndexAccess(arr, idx))
    )

    if cascade.dispatch(_.usesRefCount)(v.tpe) then
      CStmtExpr(CCompoundStmt(List(
        tempDecl,
        CAssignmentExpr(
          arrayIndexAccess(arr, idx),
          retain(
            cascade.dispatch(_.compileTermToCExpr)(v),
            v.tpe
          )
        ),
        release(arrayIndexAccess(arr, idx), v.tpe, CFalseLiteral).get
      )))
    else
      CAssignmentExpr(
        arrayIndexAccess(arr, idx),
        cascade.dispatch(_.compileTermToCExpr)(v),
      )
  }

  private def getArrayFill(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> FILL, buildArrayFill(tpe))
  }

  private def buildArrayFill(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    val recordDecl = getRecordDecl(tpe)
    val CFieldDecl(_, CQualType(CPointerType(elemCType), _)) = recordDecl.getField(dataField): @unchecked

    val typeArgs(List(elemType)) = tpe.widen: @unchecked

    val name = "fill_" + recordDecl.name

    val nParam = CParmVarDecl("n", CIntegerType)
    val elemParam = CParmVarDecl("elem", elemCType)

    val arrDecl =
      CVarDecl(
        "arr",
        recordDecl.getTypeForDecl,
        Some(arrayStructInitializer(tpe, nParam.ref))
      )

    val iDecl = CVarDecl("i", CIntegerType, Some(0.lit))

    val loop =
      CForStmt(
        Some(iDecl),
        Some(CLessThanExpr(iDecl.ref, nParam.ref)),
        Some(CIncExpr(iDecl.ref)),
        CCompoundStmt(List(
          CAssignmentExpr(
            CArraySubscriptExpr(
              CMemberExpr(arrDecl.ref, dataField),
              iDecl.ref
            ),
            retain(
              elemParam.ref,
              elemType
            )
          )
        ))
      )

    val body = CCompoundStmt(List(
      arrDecl,
      loop,
      CReturnStmt(Some(arrDecl.ref))
    ))

    CFunctionDecl(name, List(nParam, elemParam), recordDecl.getTypeForDecl, Some(body))
  }

  private def getArrayDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> DEEP_COPY, buildArrayDeepCopy(tpe))
  }

  private def buildArrayDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(elemType)) = tpe.widen: @unchecked

    val name = "deepCopy_" + recordDecl.name

    val arrayParam = CParmVarDecl("arr", recordDecl.getTypeForDecl)
    val lengthExpr = CMemberExpr(arrayParam.ref, lengthField)
    val CQualType(CPointerType(elemCType), _) = recordDecl.getField(dataField).declaredType: @unchecked

    val copyDecl =
      CVarDecl(
        "copy",
        recordDecl.getTypeForDecl,
        Some(CDesignatedInitExpr(List(
          dataField -> CCastExpr(
            CCallExpr(
              StdLibH.calloc.ref,
              List(
                lengthExpr,
                CSizeofExpr(Left(elemCType.unqualType))
              )
            ),
            CPointerType(elemCType)
          ),
          lengthField -> lengthExpr,
          allocRefCount(tpe).get
        )))
      )

    val iter = CVarDecl("i", CIntegerType, Some(0.lit))

    val loop = CForStmt(
      Some(iter),
      Some(CLessThanExpr(iter.ref, lengthExpr)),
      Some(CIncExpr(iter.ref)),
      CAssignmentExpr(
        CArraySubscriptExpr(CMemberExpr(copyDecl.ref, dataField), iter.ref),
        retain(
          CompileDataStructure.deepCopy(
            CArraySubscriptExpr(CMemberExpr(arrayParam.ref, dataField), iter.ref),
            elemType
          ),
          elemType
        )
      )
    )

    val body = CCompoundStmt(List(
      copyDecl,
      loop,
      CReturnStmt(Some(copyDecl.ref))
    ))

    CFunctionDecl(
      name,
      List(arrayParam),
      recordDecl.getTypeForDecl,
      Some(body),
    )
  }

  private def getArrayPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> PRINT, buildArrayPrinter(tpe))
  }

  private def buildArrayPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(elemType)) = tpe.widen: @unchecked

    val name = "print_" + recordDecl.name

    val arrayParam = CParmVarDecl("arr", recordDecl.getTypeForDecl)

    val iter = CVarDecl("i", CIntegerType, Some(0.lit))

    val loop = CForStmt(
      Some(iter),
      Some(CLessThanExpr(iter.ref, CMemberExpr(arrayParam.ref, lengthField))),
      Some(CIncExpr(iter.ref)),
      CCompoundStmt(List(
        CIfStmt(CGreaterThanExpr(iter.ref, 0.lit), CompileString.printf(", ")),
        cascade.dispatch(_.compilePrint)(
          CArraySubscriptExpr(
            CMemberExpr(arrayParam.ref, dataField),
            iter.ref
          ),
          elemType
        )
      ))
    )

    val body = CCompoundStmt(List(
      CompileString.printf("["),
      loop,
      CompileString.printf("]")
    ))

    CFunctionDecl(
      name,
      List(arrayParam),
      CVoidType,
      Some(body),
    )
  }

  private def getArrayToString(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> TO_STRING, buildArrayToString(tpe))
  }

  private def buildArrayToString(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(elemType)) = tpe.widen: @unchecked

    val name = "toString_" + recordDecl.name

    val arrayParam = CParmVarDecl("arr", recordDecl.getTypeForDecl)

    val elemStringsDecl = CVarDecl(
      "elemStrings",
      CArrayType(CPointerType(CCharType), Some(CMemberExpr(arrayParam.ref, lengthField)))
    )

    val stringLengthDecl = CVarDecl("strLength", CIntegerType, Some(3.lit))

    val iter = CVarDecl("i", CIntegerType, Some(0.lit))

    val elemLoop = CForStmt(
      Some(iter),
      Some(CLessThanExpr(iter.ref, CMemberExpr(arrayParam.ref, lengthField))),
      Some(CIncExpr(iter.ref)),
      CCompoundStmt(List(
        CAssignmentExpr(
          CArraySubscriptExpr(elemStringsDecl.ref, iter.ref),
          cascade.dispatch(_.compileToString)(
            CArraySubscriptExpr(CMemberExpr(arrayParam.ref, dataField), iter.ref),
            elemType
          )
        ),
        CIfStmt(CGreaterThanExpr(iter.ref, 0.lit), CPlusAssignmentExpr(stringLengthDecl.ref, 2.lit)),
        CPlusAssignmentExpr(
          stringLengthDecl.ref,
          CCallExpr(StringH.strlen.ref, List(CArraySubscriptExpr(elemStringsDecl.ref, iter.ref)))
        )
      ))
    )

    val strDecl = CVarDecl(
      "str",
      CPointerType(CCharType),
      Some(CCastExpr(
        CCallExpr(StdLibH.calloc.ref, List(stringLengthDecl.ref, CSizeofExpr(Left(CCharType)))),
        CPointerType(CCharType)
      ))
    )

    val openingBracket = CAssignmentExpr(CArraySubscriptExpr(strDecl.ref, 0.lit), CCharacterLiteral('['))

    val concatLoop = CForStmt(
      Some(iter),
      Some(CLessThanExpr(iter.ref, CMemberExpr(arrayParam.ref, lengthField))),
      Some(CIncExpr(iter.ref)),
      CCompoundStmt(List(
        CIfStmt(CGreaterThanExpr(iter.ref, 0.lit), CCallExpr(StringH.strcat.ref, List(strDecl.ref, CStringLiteral(", ")))),
        CCallExpr(StringH.strcat.ref, List(strDecl.ref, CArraySubscriptExpr(elemStringsDecl.ref, iter.ref))),
        CCallExpr(StdLibH.free.ref, List(CArraySubscriptExpr(elemStringsDecl.ref, iter.ref)))
      ))
    )

    val closingBracket = CCallExpr(StringH.strcat.ref, List(strDecl.ref, CStringLiteral("]")))

    val body = CCompoundStmt(List(
      elemStringsDecl,
      stringLengthDecl,
      elemLoop,
      strDecl,
      openingBracket,
      concatLoop,
      closingBracket,
      CReturnStmt(Some(strDecl.ref))
    ))

    CFunctionDecl(
      name,
      List(arrayParam),
      CPointerType(CCharType),
      Some(body)
    )
  }

  private def getArraySerialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> SERIALIZE, buildArraySerialize(tpe))
  }

  private def buildArraySerialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(elemType)) = tpe.widen: @unchecked

    val name = "serialize_" + recordDecl.name

    val arrayParam = CParmVarDecl("arr", recordDecl.getTypeForDecl)

    val jsonDecl = CVarDecl("json", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_CreateArray.ref, List())))

    val iDecl = CVarDecl("i", CIntegerType, Some(0.lit))
    val loop = CForStmt(
      Some(iDecl),
      Some(CLessThanExpr(iDecl.ref, CMemberExpr(arrayParam.ref, lengthField))),
      Some(CIncExpr(iDecl.ref)),
      CCallExpr(
        CJSONH.cJSON_AddItemToArray.ref,
        List(jsonDecl.ref, serialize(CArraySubscriptExpr(CMemberExpr(arrayParam.ref, dataField), iDecl.ref), elemType))
      )
    )

    val body = CCompoundStmt(List(
      jsonDecl,
      loop,
      CReturnStmt(Some(jsonDecl.ref))
    ))

    CFunctionDecl(name, List(arrayParam), CPointerType(CJSONH.cJSON), Some(body))
  }

  private def getArrayDeserialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> DESERIALIZE, buildArrayDeserialize(tpe))
  }

  private def buildArrayDeserialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(elemType)) = tpe.widen: @unchecked

    val name = "deserialize_" + recordDecl.name

    val jsonParam = CParmVarDecl("json", CPointerType(CJSONH.cJSON))

    val sizeDecl = CVarDecl("size", CIntegerType, Some(CCallExpr(CJSONH.cJSON_GetArraySize.ref, List(jsonParam.ref))))

    val arrDecl = CVarDecl(
      "arr",
      recordDecl.getTypeForDecl,
      Some(arrayStructInitializer(tpe, sizeDecl.ref))
    )

    val iDecl = CVarDecl("i", CIntegerType, Some(0.lit))
    val loop = CForStmt(
      Some(iDecl),
      Some(CLessThanExpr(iDecl.ref, sizeDecl.ref)),
      Some(CIncExpr(iDecl.ref)),
      CAssignmentExpr(
        CArraySubscriptExpr(CMemberExpr(arrDecl.ref, dataField), iDecl.ref),
        retain(deserialize(CCallExpr(CJSONH.cJSON_GetArrayItem.ref, List(jsonParam.ref, iDecl.ref)), elemType), elemType)
      )
    )

    val body = CCompoundStmt(List(
      sizeDecl,
      arrDecl,
      loop,
      CReturnStmt(Some(arrDecl.ref))
    ))

    CFunctionDecl(name, List(jsonParam), recordDecl.getTypeForDecl, Some(body))
  }
}
