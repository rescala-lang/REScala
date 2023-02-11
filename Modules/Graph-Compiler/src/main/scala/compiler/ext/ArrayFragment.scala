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
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.base.*
import compiler.base.TypeFragment.typeArgs
import compiler.base.ApplyFragment.varArgs
import compiler.base.DataStructureFragment.{release, retain}
import compiler.context.{FunctionDeclTC, RecordDeclTC, TranslationContext}
import compiler.ext.SerializationFragment.{deserialize, serialize}

import scala.quoted.*

object ArrayFragment extends SelectIFFragment with ApplyIFFragment with MatchIFFragment with TypeIFFragment
    with DataStructureIFFragment with StringIFFragment with SerializationIFFragment {
  override def compileSelect(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Select, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case Select(arr, "length") =>
        CMemberExpr(
          dispatch[TermIFFragment](_.compileTermToCExpr)(arr),
          lengthField
        )
    }
  }

  override def compileApply(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case apply @ Apply(Apply(TypeApply(Select(Ident("Array"), "fill"), _), List(n)), List(elem)) =>
        CCallExpr(
          getArrayFill(apply.tpe).ref,
          List(
            dispatch[TermIFFragment](_.compileTermToCExpr)(n),
            dispatch[TermIFFragment](_.compileTermToCExpr)(elem)
          )
        )
      case apply @ Apply(TypeApply(Select(Ident("Array"), "ofDim"), List(tpt)), List(n))
          if TypeFragment.hasDefaultValue(tpt.tpe) =>
        CCallExpr(
          getArrayFill(apply.tpe).ref,
          List(
            dispatch[TermIFFragment](_.compileTermToCExpr)(n),
            dispatch[TypeIFFragment](_.defaultValue)(tpt.tpe)
          )
        )
      case apply @ this.arrayApply(args) =>
        val elems = args.map(dispatch[TermIFFragment](_.compileTermToCExpr))

        CCallExpr(getArrayCreator(apply.tpe).ref, CIntegerLiteral(elems.length) :: elems)
      case Apply(Select(arr, "apply"), List(idx)) if arr.tpe <:< TypeRepr.of[Array[?]] =>
        arrayIndexAccess(arr, idx)
      case Apply(Select(arr, "update"), List(idx, v)) if arr.tpe <:< TypeRepr.of[Array[?]] =>
        arrayIndexUpdate(arr, idx, v)
      case Apply(Apply(TypeApply(Ident("deepCopy"), _), List(arr)), List()) if arr.tpe <:< TypeRepr.of[Array[?]] =>
        CCallExpr(
          getArrayDeepCopy(arr.tpe).ref,
          List(dispatch[TermIFFragment](_.compileTermToCExpr)(arr))
        )
    }
  }

  override def compilePattern(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] =
    ensureCtx[RecordDeclTC] {
      import quotes.reflect.*

      {
        case (Unapply(TypeApply(Select(Ident("Array"), "unapplySeq"), _), _, subPatterns), prefix, prefixType) =>
          val typeArgs(List(elemType)) = prefixType.widen: @unchecked

          val lengthCond = CEqualsExpr(CMemberExpr(prefix, lengthField), CIntegerLiteral(subPatterns.length))

          subPatterns.zipWithIndex.foldLeft((Option[CExpr](lengthCond), List.empty[CVarDecl])) {
            case ((cond, decls), (subPattern, i)) =>
              val (subCond, subDecls) = dispatch[MatchIFFragment](_.compilePattern)(
                subPattern,
                CArraySubscriptExpr(CMemberExpr(prefix, dataField), i.lit),
                elemType
              )

              val combinedCond = MatchFragment.combineCond(cond, subCond)

              (combinedCond, subDecls ++ decls)
          }
      }
    }

  override def compileEquals(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] =
    ensureCtx[RecordDeclTC] {
      import quotes.reflect.*

      {
        case (leftExpr, leftType, rightExpr, _) if leftType <:< TypeRepr.of[Array[?]] =>
          CParenExpr(CEqualsExpr(
            CMemberExpr(leftExpr, dataField),
            CMemberExpr(rightExpr, dataField)
          ))
      }
    }

  override def compileTypeRepr(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Array[?]] =>
        getRecordDecl(tpe).getTypeForDecl
    }
  }

  override def typeName(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, String] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Array[?]] => dispatch[TypeIFFragment](_.classTypeName)(tpe)
    }
  }

  override def defaultValue(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Array[?]] =>
        CCallExpr(getArrayCreator(tpe).ref, List())
    }
  }

  override def compileTypeToCRecordDecl(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Array[?]] =>
        val typeArgs(List(elemType)) = tpe.widen: @unchecked

        val dataFieldDecl   = CFieldDecl(dataField, CPointerType(dispatch[TypeIFFragment](_.compileTypeRepr)(elemType)))
        val lengthFieldDecl = CFieldDecl(lengthField, CIntegerType)
        val refCountFieldDecl = CFieldDecl(refCountFieldName, CPointerType(CIntegerType))

        CRecordDecl(
          "Array_" + dispatch[TypeIFFragment](_.typeName)(elemType),
          List(dataFieldDecl, lengthFieldDecl, refCountFieldDecl)
        )
    }
  }

  override def usesRefCount(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Array[?]] => true
    }
  }

  override def compileFree(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[Array[?]] =>
        val typeArgs(List(elemType)) = tpe.widen: @unchecked

        val propagateRelease = dispatch[DataStructureIFFragment](_.usesRefCount)(elemType)

        val freeThis: List[CStmt] = List(
          CCallExpr(StdLibH.free.ref, List(CMemberExpr(expr, refCountFieldName))),
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

  override def compileDeepCopy(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Array[?]] =>
        getArrayDeepCopy(tpe)
    }
  }

  override def compilePrint(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[Array[?]] =>
        CCallExpr(getArrayPrinter(tpe).ref, List(expr))
    }
  }

  override def compileToString(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[Array[?]] =>
        CCallExpr(getArrayToString(tpe).ref, List(expr))
    }
  }

  override def serializationRetainsEquality(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Array[?]] =>
        val typeArgs(List(elemType)) = tpe.widen: @unchecked
        dispatch[SerializationIFFragment](_.serializationRetainsEquality)(elemType)
    }
  }

  override def compileSerialize(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Array[?]] =>
        getArraySerialize(tpe)
    }
  }

  override def compileDeserialize(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Array[?]] =>
        getArrayDeserialize(tpe)
    }
  }

  private val dataField: String   = "data"
  private val lengthField: String = "length"

  private def arrayApply(using Quotes): PartialFunction[quotes.reflect.Apply, List[quotes.reflect.Term]] = {
    import quotes.reflect.*

    {
      case Apply(Select(Ident("Array"), "apply"), varArgs(args))               => args
      case Apply(TypeApply(Select(Ident("Array"), "apply"), _), varArgs(args)) => args
    }
  }

  private val CREATE    = "CREATE"
  private val FILL      = "FILL"
  private val PRINT     = "PRINT"
  private val TO_STRING = "TO_STRING"

  private def arrayStructInitializer(using Quotes)(tpe: quotes.reflect.TypeRepr, sizeExpr: CExpr)(using
      FragmentedCompiler
  )(using RecordDeclTC): CExpr = {
    import quotes.reflect.*

    val recordDecl                                           = getRecordDecl(tpe)
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

  private def getArrayCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> CREATE, {
        import quotes.reflect.*

        val recordDecl               = getRecordDecl(tpe)
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
    )
  }

  private def arrayIndexAccess(using Quotes)(arr: quotes.reflect.Term, idx: quotes.reflect.Term)(using
      FragmentedCompiler
  )(using TranslationContext): CArraySubscriptExpr = {
    CArraySubscriptExpr(
      CMemberExpr(
        dispatch[TermIFFragment](_.compileTermToCExpr)(arr),
        dataField
      ),
      dispatch[TermIFFragment](_.compileTermToCExpr)(idx)
    )
  }

  private def arrayIndexUpdate(using
      Quotes
  )(arr: quotes.reflect.Term, idx: quotes.reflect.Term, v: quotes.reflect.Term)(using FragmentedCompiler)(using
      TranslationContext
  ): CExpr = {
    val tempDecl = CVarDecl(
      "temp",
      dispatch[TypeIFFragment](_.compileTypeRepr)(v.tpe),
      Some(arrayIndexAccess(arr, idx))
    )

    if dispatch[DataStructureIFFragment](_.usesRefCount)(v.tpe) then
      CStmtExpr(CCompoundStmt(List(
        tempDecl,
        CAssignmentExpr(
          arrayIndexAccess(arr, idx),
          retain(
            dispatch[TermIFFragment](_.compileTermToCExpr)(v),
            v.tpe
          )
        ),
        release(tempDecl.ref, v.tpe, CFalseLiteral).get
      )))
    else
      CAssignmentExpr(
        arrayIndexAccess(arr, idx),
        dispatch[TermIFFragment](_.compileTermToCExpr)(v),
      )
  }

  private def getArrayFill(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> FILL, {
        val recordDecl                                           = getRecordDecl(tpe)
        val CFieldDecl(_, CQualType(CPointerType(elemCType), _)) = recordDecl.getField(dataField): @unchecked

        val typeArgs(List(elemType)) = tpe.widen: @unchecked

        val name = "fill_" + recordDecl.name

        val nParam    = CParmVarDecl("n", CIntegerType)
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
    )
  }

  private def getArrayDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> DEEP_COPY, {
        import quotes.reflect.*

        val recordDecl               = getRecordDecl(tpe)
        val typeArgs(List(elemType)) = tpe.widen: @unchecked

        val name = "deepCopy_" + recordDecl.name

        val arrayParam                            = CParmVarDecl("arr", recordDecl.getTypeForDecl)
        val lengthExpr                            = CMemberExpr(arrayParam.ref, lengthField)
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
              DataStructureFragment.deepCopy(
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
    )
  }

  private def getArrayPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> PRINT, {
        import quotes.reflect.*

        val recordDecl               = getRecordDecl(tpe)
        val typeArgs(List(elemType)) = tpe.widen: @unchecked

        val name = "print_" + recordDecl.name

        val arrayParam = CParmVarDecl("arr", recordDecl.getTypeForDecl)

        val iter = CVarDecl("i", CIntegerType, Some(0.lit))

        val loop = CForStmt(
          Some(iter),
          Some(CLessThanExpr(iter.ref, CMemberExpr(arrayParam.ref, lengthField))),
          Some(CIncExpr(iter.ref)),
          CCompoundStmt(List(
            CIfStmt(CGreaterThanExpr(iter.ref, 0.lit), StringFragment.printf(", ")),
            dispatch[StringIFFragment](_.compilePrint)(
              CArraySubscriptExpr(
                CMemberExpr(arrayParam.ref, dataField),
                iter.ref
              ),
              elemType
            )
          ))
        )

        val body = CCompoundStmt(List(
          StringFragment.printf("["),
          loop,
          StringFragment.printf("]")
        ))

        CFunctionDecl(
          name,
          List(arrayParam),
          CVoidType,
          Some(body),
        )
      }
    )
  }

  private def getArrayToString(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> TO_STRING, {
        import quotes.reflect.*

        val recordDecl               = getRecordDecl(tpe)
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
              dispatch[StringIFFragment](_.compileToString)(
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
            CIfStmt(
              CGreaterThanExpr(iter.ref, 0.lit),
              CCallExpr(StringH.strcat.ref, List(strDecl.ref, CStringLiteral(", ")))
            ),
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
    )
  }

  private def getArraySerialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> SERIALIZE, {
        import quotes.reflect.*

        val recordDecl               = getRecordDecl(tpe)
        val typeArgs(List(elemType)) = tpe.widen: @unchecked

        val name = "serialize_" + recordDecl.name

        val arrayParam = CParmVarDecl("arr", recordDecl.getTypeForDecl)

        val jsonDecl =
          CVarDecl("json", CPointerType(CJSONH.cJSON), Some(CCallExpr(CJSONH.cJSON_CreateArray.ref, List())))

        val iDecl = CVarDecl("i", CIntegerType, Some(0.lit))
        val loop = CForStmt(
          Some(iDecl),
          Some(CLessThanExpr(iDecl.ref, CMemberExpr(arrayParam.ref, lengthField))),
          Some(CIncExpr(iDecl.ref)),
          CCallExpr(
            CJSONH.cJSON_AddItemToArray.ref,
            List(
              jsonDecl.ref,
              serialize(CArraySubscriptExpr(CMemberExpr(arrayParam.ref, dataField), iDecl.ref), elemType)
            )
          )
        )

        val body = CCompoundStmt(List(
          jsonDecl,
          loop,
          CReturnStmt(Some(jsonDecl.ref))
        ))

        CFunctionDecl(name, List(arrayParam), CPointerType(CJSONH.cJSON), Some(body))
      }
    )
  }

  private def getArrayDeserialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> DESERIALIZE, {
        import quotes.reflect.*

        val recordDecl               = getRecordDecl(tpe)
        val typeArgs(List(elemType)) = tpe.widen: @unchecked

        val name = "deserialize_" + recordDecl.name

        val jsonParam = CParmVarDecl("json", CPointerType(CJSONH.cJSON))

        val sizeDecl =
          CVarDecl("size", CIntegerType, Some(CCallExpr(CJSONH.cJSON_GetArraySize.ref, List(jsonParam.ref))))

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
            retain(
              deserialize(CCallExpr(CJSONH.cJSON_GetArrayItem.ref, List(jsonParam.ref, iDecl.ref)), elemType),
              elemType
            )
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
    )
  }
}
