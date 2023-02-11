package compiler.ext

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.binaryop.{CAndExpr, CAssignmentExpr, CEqualsExpr, CNotEqualsExpr, COrExpr, CPlusExpr}
import clangast.expr.unaryop.{CAddressExpr, CDerefExpr, CNotExpr}
import clangast.expr.*
import clangast.stmt.{CCompoundStmt, CIfStmt, CReturnStmt, CStmt}
import clangast.stubs.{CJSONH, StdBoolH, StdLibH, StringH}
import clangast.types.*
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.base.*
import compiler.base.TypeFragment.typeArgs
import compiler.base.DataStructureFragment.{release, retain}
import compiler.ext.SerializationFragment.{serialize, deserialize}
import compiler.context.{RecordDeclTC, TranslationContext, ValueDeclTC}

import scala.quoted.*

object OptionFragment extends DefinitionIFFragment with TermIFFragment with SelectIFFragment with ApplyIFFragment
    with MatchIFFragment with TypeIFFragment with DataStructureIFFragment with StringIFFragment
    with SerializationIFFragment {
  override def compileValDefToCVarDecl(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.ValDef, CVarDecl] = ensureCtx[RecordDeclTC & ValueDeclTC] { ctx ?=>
    import quotes.reflect.*

    {
      case ValDef(name, tpt, Some(Ident("None"))) =>
        val init = retain(CCallExpr(getNoneCreator(tpt.tpe).ref, List()), tpt.tpe)
        ctx.registerValueName(name)
        val decl = CVarDecl(name, dispatch[TypeIFFragment](_.compileTypeRepr)(tpt.tpe), Some(init))
        ctx.nameToDecl.put(name, decl)
        decl
    }
  }

  override def compileTerm(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Term, CASTNode] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case t @ TypeApply(Select(Ident("Option"), "empty"), _) =>
        CCallExpr(
          getNoneCreator(t.tpe).ref,
          List()
        )
      case Apply(TypeApply(Select(opt, "foreach"), _), List(f)) if opt.tpe <:< TypeRepr.of[Option[?]] =>
        compileForeach(opt, f)
    }
  }

  override def compileSelect(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Select, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case Select(opt, "get") if opt.tpe <:< TypeRepr.of[Option[?]] =>
        CMemberExpr(dispatch[TermIFFragment](_.compileTermToCExpr)(opt), valField)
      case Select(opt, "isDefined") if opt.tpe <:< TypeRepr.of[Option[?]] =>
        CMemberExpr(dispatch[TermIFFragment](_.compileTermToCExpr)(opt), definedField)
      case Select(opt, "isEmpty") if opt.tpe <:< TypeRepr.of[Option[?]] =>
        CNotExpr(CMemberExpr(dispatch[TermIFFragment](_.compileTermToCExpr)(opt), definedField))
    }
  }

  override def compileApply(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case Apply(TypeApply(Select(Ident("Some"), "apply"), _), List(inner)) =>
        CCallExpr(
          getSomeCreator(TypeRepr.of[Option].appliedTo(inner.tpe)).ref,
          List(dispatch[TermIFFragment](_.compileTermToCExpr)(inner))
        )
      case Apply(TypeApply(Select(opt, "getOrElse"), _), List(defaultVal)) if opt.tpe <:< TypeRepr.of[Option[?]] =>
        CCallExpr(
          getOptionGetOrElse(opt.tpe).ref,
          List(
            dispatch[TermIFFragment](_.compileTermToCExpr)(opt),
            dispatch[TermIFFragment](_.compileTermToCExpr)(defaultVal)
          )
        )
      case Apply(Apply(TypeApply(Ident("deepCopy"), _), List(opt)), List()) if opt.tpe <:< TypeRepr.of[Option[?]] =>
        CCallExpr(
          getOptionDeepCopy(opt.tpe).ref,
          List(dispatch[TermIFFragment](_.compileTermToCExpr)(opt))
        )
      case apply @ Apply(TypeApply(Select(opt, "map"), _), List(f)) if opt.tpe <:< TypeRepr.of[Option[?]] =>
        compileMap(opt, f, apply.tpe)
      case Apply(Select(opt, "filter"), List(f)) if opt.tpe <:< TypeRepr.of[Option[?]] =>
        compileFilter(opt, f)
    }
  }

  override def compileEquals(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr, CExpr, quotes.reflect.TypeRepr), CExpr] =
    ensureCtx[RecordDeclTC] {
      import quotes.reflect.*

      {
        case (leftExpr, leftType, rightExpr, _) if leftType <:< TypeRepr.of[Option[?]] =>
          CCallExpr(
            getOptionEquals(leftType).ref,
            List(leftExpr, rightExpr)
          )
      }
    }

  override def compilePattern(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] =
    ensureCtx[RecordDeclTC] {
      import quotes.reflect.*

      {
        case (Ident("None"), prefix, _) =>
          val cond = CNotExpr(CMemberExpr(prefix, definedField))
          (Some(cond), List())
        case (
              TypedOrTest(Unapply(TypeApply(Select(Ident("Some"), "unapply"), _), _, List(subPattern)), _),
              prefix,
              prefixType
            ) =>
          val subPrefix                     = CMemberExpr(prefix, valField)
          val typeArgs(List(subPrefixType)) = prefixType.widen: @unchecked

          val definedCond = CMemberExpr(prefix, definedField)

          val (subCond, subDecls) = dispatch[MatchIFFragment](_.compilePattern)(subPattern, subPrefix, subPrefixType)

          val combinedCond = MatchFragment.combineCond(Some(definedCond), subCond)

          (combinedCond, subDecls)
      }
    }

  override def compileTypeRepr(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CType] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Option[?]] =>
        getSomeCreator(tpe)
        getNoneCreator(tpe)
        getRecordDecl(tpe).getTypeForDecl
    }
  }

  override def typeName(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, String] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Option[?]] =>
        val typeArgs(List(wrappedType)) = tpe.widen: @unchecked
        dispatch[TypeIFFragment](_.classTypeName)(TypeRepr.of[Option].appliedTo(wrappedType))
    }
  }

  override def defaultValue(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Option[?]] && !dispatch[DataStructureIFFragment](_.usesRefCount)(tpe) =>
        CCallExpr(getNoneCreator(tpe).ref, List())
    }
  }

  override def compileTypeToCRecordDecl(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Option[?]] =>
        val typeArgs(List(wrappedType)) = tpe.widen: @unchecked

        val valFieldDecl     = CFieldDecl(valField, dispatch[TypeIFFragment](_.compileTypeRepr)(wrappedType))
        val definedFieldDecl = CFieldDecl(definedField, StdBoolH.bool)

        val refCountFieldDecl =
          if dispatch[DataStructureIFFragment](_.usesRefCount)(wrappedType) then
            List(CFieldDecl(refCountFieldName, CPointerType(CIntegerType)))
          else Nil

        CRecordDecl(
          "Option_" + dispatch[TypeIFFragment](_.typeName)(wrappedType),
          List(valFieldDecl, definedFieldDecl) ++ refCountFieldDecl
        )
    }
  }

  override def usesRefCount(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Option[?]] =>
        val typeArgs(List(wrappedType)) = tpe.widen: @unchecked
        dispatch[DataStructureIFFragment](_.usesRefCount)(wrappedType)
    }
  }

  override def compileFree(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[Option[?]] =>
        val typeArgs(List(wrappedType)) = tpe.widen: @unchecked

        CCompoundStmt(List(
          CIfStmt(
            CMemberExpr(expr, definedField),
            release(CMemberExpr(expr, valField), wrappedType, CFalseLiteral).get
          ),
          CCallExpr(StdLibH.free.ref, List(CMemberExpr(expr, refCountFieldName)))
        ))
    }
  }

  override def compileDeepCopy(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Option[?]] && dispatch[DataStructureIFFragment](_.usesRefCount)(tpe) =>
        getOptionDeepCopy(tpe)
    }
  }

  override def compilePrint(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[Option[?]] =>
        CCallExpr(getOptionPrinter(tpe).ref, List(expr))
    }
  }

  override def compileToString(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case (expr, tpe) if tpe <:< TypeRepr.of[Option[?]] =>
        CCallExpr(getOptionToString(tpe).ref, List(expr))
    }
  }

  override def serializationRetainsEquality(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Option[?]] =>
        val typeArgs(List(wrappedType)) = tpe.widen: @unchecked
        dispatch[SerializationIFFragment](_.serializationRetainsEquality)(wrappedType)
    }
  }

  override def compileSerialize(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Option[?]] =>
        getOptionSerialize(tpe)
    }
  }

  override def compileDeserialize(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] {
    import quotes.reflect.*

    {
      case tpe if tpe <:< TypeRepr.of[Option[?]] =>
        getOptionDeserialize(tpe)
    }
  }

  val valField     = "val"
  val definedField = "defined"

  private val CREATE_NONE = "CREATE_NONE"
  private val CREATE_SOME = "CREATE_SOME"
  private val GET_OR_ELSE = "GET_OR_ELSE"
  private val EQUALS      = "EQUALS"
  private val PRINT       = "PRINT"
  private val TO_STRING   = "TO_STRING"

  def getNoneCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> CREATE_NONE, {
        val recordDecl = getRecordDecl(tpe)
        val name       = "createNone_" + recordDecl.name

        val optDecl = CVarDecl(
          "opt",
          recordDecl.getTypeForDecl,
          Some(CDesignatedInitExpr(
            (definedField -> CFalseLiteral) :: allocRefCount(tpe).toList
          ))
        )

        val body = CCompoundStmt(List(
          optDecl,
          CReturnStmt(Some(optDecl.ref))
        ))

        CFunctionDecl(name, List(), recordDecl.getTypeForDecl, Some(body))
      }
    )
  }

  def getSomeCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> CREATE_SOME, {
        val typeArgs(List(wrappedType)) = tpe.widen: @unchecked
        val recordDecl                  = getRecordDecl(tpe)
        val name                        = "createSome_" + recordDecl.name

        val valParam = CParmVarDecl("val", recordDecl.getField(valField).declaredType)

        val optDecl = CVarDecl(
          "opt",
          recordDecl.getTypeForDecl,
          Some(CDesignatedInitExpr(
            List(
              valField     -> retain(valParam.ref, wrappedType),
              definedField -> CTrueLiteral
            ) ++ allocRefCount(tpe)
          ))
        )

        val body = CCompoundStmt(List(
          optDecl,
          CReturnStmt(Some(optDecl.ref))
        ))

        CFunctionDecl(name, List(valParam), recordDecl.getTypeForDecl, Some(body))
      }
    )
  }

  def getOptionGetOrElse(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> GET_OR_ELSE, {
        val recordDecl = getRecordDecl(tpe)
        val name       = "getOrElse_" + recordDecl.name

        val optParam        = CParmVarDecl("opt", recordDecl.getTypeForDecl)
        val valType         = recordDecl.getField(valField).declaredType
        val defaultValParam = CParmVarDecl("defaultVal", valType)

        val body = CCompoundStmt(List(
          CReturnStmt(Some(
            CConditionalOperator(
              CMemberExpr(optParam.ref, definedField),
              CMemberExpr(optParam.ref, valField),
              defaultValParam.ref
            )
          ))
        ))

        CFunctionDecl(name, List(optParam, defaultValParam), valType, Some(body))
      }
    )
  }

  private def getOptionEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> EQUALS, {
        import quotes.reflect.*

        val recordDecl                  = getRecordDecl(tpe)
        val typeArgs(List(wrappedType)) = tpe.widen: @unchecked

        val name = "equals_" + recordDecl.name

        val paramLeft  = CParmVarDecl("left", recordDecl.getTypeForDecl)
        val paramRight = CParmVarDecl("right", recordDecl.getTypeForDecl)

        val equalsExpr = COrExpr(
          CAndExpr(
            CNotExpr(CMemberExpr(paramLeft.ref, definedField)),
            CNotExpr(CMemberExpr(paramRight.ref, definedField))
          ),
          CAndExpr(
            CAndExpr(
              CMemberExpr(paramLeft.ref, definedField),
              CMemberExpr(paramRight.ref, definedField)
            ),
            dispatch[ApplyIFFragment](_.compileEquals)(
              CMemberExpr(paramLeft.ref, valField),
              wrappedType,
              CMemberExpr(paramRight.ref, valField),
              wrappedType,
            )
          )
        )

        val body = CCompoundStmt(List(CReturnStmt(Some(equalsExpr))))

        CFunctionDecl(name, List(paramLeft, paramRight), StdBoolH.bool, Some(body))
      }
    )
  }

  private def getOptionDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> DEEP_COPY, {
        import quotes.reflect.*

        val recordDecl                  = getRecordDecl(tpe)
        val typeArgs(List(wrappedType)) = tpe.widen: @unchecked

        val name = "deepCopy_" + recordDecl.name

        val optParam = CParmVarDecl("opt", recordDecl.getTypeForDecl)

        val body = CCompoundStmt(List(
          CIfStmt(
            CMemberExpr(optParam.ref, definedField),
            CReturnStmt(Some(CCallExpr(
              getSomeCreator(tpe).ref,
              List(DataStructureFragment.deepCopy(
                CMemberExpr(optParam.ref, valField),
                wrappedType
              ))
            ))),
            Some(CReturnStmt(Some(CCallExpr(
              getNoneCreator(tpe).ref,
              List()
            ))))
          )
        ))

        CFunctionDecl(name, List(optParam), recordDecl.getTypeForDecl, Some(body))
      }
    )
  }

  private def compileMap(using
      Quotes
  )(opt: quotes.reflect.Term, f: quotes.reflect.Term, resType: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CExpr = {
    import quotes.reflect.*

    val compiledF = dispatch[TermIFFragment](_.compileTerm)(f) match {
      case funDecl: CFunctionDecl => funDecl
    }

    val recordDecl = getRecordDecl(opt.tpe)

    val tempDecl = CVarDecl(
      ctx.uniqueValueName("_temp"),
      recordDecl.getTypeForDecl,
      Some(dispatch[TermIFFragment](_.compileTermToCExpr)(opt))
    )
    val resDecl = CVarDecl(ctx.uniqueValueName("_res"), dispatch[TypeIFFragment](_.compileTypeRepr)(resType))

    CStmtExpr(CCompoundStmt(List(
      tempDecl,
      resDecl,
      CIfStmt(
        CMemberExpr(tempDecl.ref, definedField),
        CAssignmentExpr(
          resDecl.ref,
          CCallExpr(
            getSomeCreator(resType).ref,
            List(compiledF.inlineCall(List(CMemberExpr(tempDecl.ref, valField))))
          )
        ),
        Some(CAssignmentExpr(resDecl.ref, CCallExpr(getNoneCreator(resType).ref, List())))
      ),
      resDecl.ref
    )))
  }

  private def compileFilter(using Quotes)(opt: quotes.reflect.Term, f: quotes.reflect.Term)(using
      FragmentedCompiler
  )(using ctx: RecordDeclTC): CExpr = {
    import quotes.reflect.*

    val compiledF = dispatch[TermIFFragment](_.compileTerm)(f) match {
      case funDecl: CFunctionDecl => funDecl
    }

    val recordDecl = getRecordDecl(opt.tpe)

    val tempDecl = CVarDecl(
      ctx.uniqueValueName("_temp"),
      recordDecl.getTypeForDecl,
      Some(dispatch[TermIFFragment](_.compileTermToCExpr)(opt))
    )
    val resDecl = CVarDecl(ctx.uniqueValueName("_res"), recordDecl.getTypeForDecl)

    CStmtExpr(CCompoundStmt(List(
      tempDecl,
      resDecl,
      CIfStmt(
        CAndExpr(
          CMemberExpr(tempDecl.ref, definedField),
          compiledF.inlineCall(List(CMemberExpr(tempDecl.ref, valField)))
        ),
        CAssignmentExpr(resDecl.ref, tempDecl.ref),
        Some(CAssignmentExpr(resDecl.ref, CCallExpr(getNoneCreator(opt.tpe).ref, List())))
      ),
      resDecl.ref
    )))
  }

  private def compileForeach(using Quotes)(opt: quotes.reflect.Term, f: quotes.reflect.Term)(using
      FragmentedCompiler
  )(using ctx: RecordDeclTC): CStmt = {
    import quotes.reflect.*

    val compiledF = dispatch[TermIFFragment](_.compileTerm)(f) match {
      case funDecl: CFunctionDecl => funDecl
    }

    val recordDecl = getRecordDecl(opt.tpe)

    val tempDecl = CVarDecl(
      ctx.uniqueValueName("_temp"),
      recordDecl.getTypeForDecl,
      Some(dispatch[TermIFFragment](_.compileTermToCExpr)(opt))
    )

    CCompoundStmt(List(
      tempDecl,
      CIfStmt(
        CMemberExpr(tempDecl.ref, definedField),
        compiledF.inlineCall(List(CMemberExpr(tempDecl.ref, valField)))
      )
    ))
  }

  private def getOptionPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> PRINT, {
        import quotes.reflect.*

        val recordDecl                  = getRecordDecl(tpe)
        val typeArgs(List(wrappedType)) = tpe.widen: @unchecked

        val name = "print_" + recordDecl.name

        val optParam = CParmVarDecl("opt", recordDecl.getTypeForDecl)

        val body = CCompoundStmt(List(
          CIfStmt(
            CMemberExpr(optParam.ref, definedField),
            CCompoundStmt(List(
              StringFragment.printf("Some("),
              dispatch[StringIFFragment](_.compilePrint)(
                CMemberExpr(optParam.ref, valField),
                wrappedType
              ),
              StringFragment.printf(")")
            )),
            Some(StringFragment.printf("None"))
          )
        ))

        CFunctionDecl(name, List(optParam), CVoidType, Some(body))
      }
    )
  }

  private def getOptionToString(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> TO_STRING, {
        import quotes.reflect.*

        val recordDecl                  = getRecordDecl(tpe)
        val typeArgs(List(wrappedType)) = tpe.widen: @unchecked

        val name = "toString_" + recordDecl.name

        val optParam = CParmVarDecl("opt", recordDecl.getTypeForDecl)

        val valStringDecl = CVarDecl(
          "valStr",
          CPointerType(CCharType),
          Some(dispatch[StringIFFragment](_.compileToString)(CMemberExpr(optParam.ref, valField), wrappedType))
        )

        val someStringDecl = StringFragment.stringDecl(
          "str",
          CPlusExpr(6.lit, CCallExpr(StringH.strlen.ref, List(valStringDecl.ref)))
        )

        val someBranch = CCompoundStmt(List(
          valStringDecl,
          someStringDecl,
          StringFragment.sprintf(someStringDecl.ref, "Some(%s)", valStringDecl.ref),
          CCallExpr(StdLibH.free.ref, List(valStringDecl.ref)),
          CReturnStmt(Some(someStringDecl.ref))
        ))

        val noneStringDecl = StringFragment.stringDecl("str", 4.lit)

        val noneBranch = CCompoundStmt(List(
          noneStringDecl,
          StringFragment.sprintf(noneStringDecl.ref, "None"),
          CReturnStmt(Some(noneStringDecl.ref))
        ))

        val body = CCompoundStmt(List(
          CIfStmt(
            CMemberExpr(optParam.ref, definedField),
            someBranch,
            Some(noneBranch)
          )
        ))

        CFunctionDecl(name, List(optParam), CPointerType(CCharType), Some(body))
      }
    )
  }

  private def getOptionSerialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> SERIALIZE, {
        import quotes.reflect.*

        val recordDecl                  = getRecordDecl(tpe)
        val typeArgs(List(wrappedType)) = tpe.widen: @unchecked

        val name = "serialize_" + recordDecl.name

        val optParam = CParmVarDecl("opt", recordDecl.getTypeForDecl)

        val someBranch = CReturnStmt(Some(serialize(CMemberExpr(optParam.ref, valField), wrappedType)))

        val noneBranch = CReturnStmt(Some(CCallExpr(CJSONH.cJSON_CreateNull.ref, List())))

        val body = CCompoundStmt(List(
          CIfStmt(
            CMemberExpr(optParam.ref, definedField),
            someBranch,
            Some(noneBranch)
          )
        ))

        CFunctionDecl(name, List(optParam), CPointerType(CJSONH.cJSON), Some(body))
      }
    )
  }

  private def getOptionDeserialize(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe) -> DESERIALIZE, {
        import quotes.reflect.*

        val recordDecl                  = getRecordDecl(tpe)
        val typeArgs(List(wrappedType)) = tpe.widen: @unchecked

        val name = "deserialize_" + recordDecl.name

        val jsonParam = CParmVarDecl("json", CPointerType(CJSONH.cJSON))

        val someBranch = CReturnStmt(Some(
          CCallExpr(
            getSomeCreator(tpe).ref,
            List(deserialize(jsonParam.ref, wrappedType))
          )
        ))

        val noneBranch = CReturnStmt(Some(CCallExpr(getNoneCreator(tpe).ref, List())))

        val body = CCompoundStmt(List(
          CIfStmt(
            CCallExpr(CJSONH.cJSON_IsNull.ref, List(jsonParam.ref)),
            noneBranch,
            Some(someBranch)
          )
        ))

        CFunctionDecl(name, List(jsonParam), recordDecl.getTypeForDecl, Some(body))
      }
    )
  }
}
