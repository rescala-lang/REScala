package compiler.ext

import clangast.{CASTNode, given}
import clangast.decl.*
import clangast.expr.binaryop.{CAndExpr, CEqualsExpr, CNotEqualsExpr, COrExpr}
import clangast.expr.unaryop.{CDerefExpr, CNotExpr}
import clangast.expr.*
import clangast.stmt.{CCompoundStmt, CIfStmt, CReturnStmt, CStmt}
import clangast.stubs.{StdBoolH, StdLibH}
import clangast.types.*
import compiler.CompilerCascade
import compiler.base.*
import compiler.base.CompileType.typeArgs
import compiler.base.CompileDataStructure.{release, retain}
import compiler.context.{RecordDeclTC, TranslationContext, ValueDeclTC}

import scala.quoted.*

object CompileOption extends DefinitionPC with TermPC with SelectPC with ApplyPC with MatchPC with TypePC with DataStructurePC with StringPC {
  private def compileValDefToCVarDeclImpl(using Quotes)(using ctx: RecordDeclTC & ValueDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = {
      import quotes.reflect.*

      {
        case ValDef(name, tpt, Some(Ident("None"))) =>
          val init = retain(CCallExpr(getNoneCreator(tpt.tpe).ref, List()), tpt.tpe)
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
        case t @ TypeApply(Select(Ident("Option"), "empty"), _) =>
          CCallExpr(
            getNoneCreator(t.tpe).ref,
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
          CMemberExpr(cascade.dispatch(_.compileTermToCExpr)(opt), valField)
        case Select(opt, "isDefined") if opt.tpe <:< TypeRepr.of[Option[?]] =>
          CMemberExpr(cascade.dispatch(_.compileTermToCExpr)(opt), definedField)
        case Select(opt, "isEmpty") if opt.tpe <:< TypeRepr.of[Option[?]] =>
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
            getSomeCreator(TypeRepr.of[Option].appliedTo(inner.tpe)).ref,
            List(cascade.dispatch(_.compileTermToCExpr)(inner))
          )
        case Apply(TypeApply(Select(opt, "getOrElse"), _), List(defaultVal)) if opt.tpe <:< TypeRepr.of[Option[?]] =>
          CCallExpr(
            getGetOrElse(opt.tpe).ref,
            List(
              cascade.dispatch(_.compileTermToCExpr)(opt),
              cascade.dispatch(_.compileTermToCExpr)(defaultVal)
            )
          )
        case Apply(Apply(TypeApply(Ident("deepCopy"), _), List(opt)), List()) if opt.tpe <:< TypeRepr.of[Option[?]] =>
          CCallExpr(
            getOptionDeepCopy(opt.tpe).ref,
            List(cascade.dispatch(_.compileTermToCExpr)(opt))
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
            getOptionEquals(leftType).ref,
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
        case (Ident("None"), prefix, _) =>
          val cond = CNotExpr(CMemberExpr(prefix, definedField))
          (Some(cond), List())
        case (TypedOrTest(Unapply(TypeApply(Select(Ident("Some"), "unapply"), _), _, List(subPattern)), _), prefix, prefixType) =>
          val subPrefix = CMemberExpr(prefix, valField)
          val typeArgs(List(subPrefixType)) = prefixType.widen

          val definedCond = CMemberExpr(prefix, definedField)

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
          getRecordDecl(tpe).getTypeForDecl
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

  override def compileTypeToCRecordDecl(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = {
      import quotes.reflect.*
  
      {
        case tpe if tpe <:< TypeRepr.of[Option[?]] =>
          val typeArgs(List(wrappedType)) = tpe

          val valFieldDecl = CFieldDecl(valField, cascade.dispatch(_.compileTypeRepr)(wrappedType))
          val definedFieldDecl = CFieldDecl(definedField, StdBoolH.bool)

          val refCountFieldDecl =
            if cascade.dispatch(_.usesRefCount)(wrappedType) then
              List(CFieldDecl(refCountField, CPointerType(CIntegerType)))
            else Nil

          CRecordDecl(
            "Option_" + cascade.dispatch(_.typeName)(wrappedType),
            List(valFieldDecl, definedFieldDecl) ++ refCountFieldDecl
          )
      }
    }

  override def usesRefCount(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
      import quotes.reflect.*

      {
        case tpe if tpe <:< TypeRepr.of[Option[?]] =>
          val typeArgs(List(wrappedType)) = tpe.widen
          cascade.dispatch(_.usesRefCount)(wrappedType)
      }
    }

  private def compileFreeImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = {
      import quotes.reflect.*
  
      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Option[?]] =>
          val typeArgs(List(wrappedType)) = tpe.widen

          CCompoundStmt(List(
            CIfStmt(
              CMemberExpr(expr, definedField),
              release(CMemberExpr(expr, valField), wrappedType, CFalseLiteral).get
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
        case tpe if tpe <:< TypeRepr.of[Option[?]] && cascade.dispatch(_.usesRefCount)(tpe) =>
          getOptionDeepCopy(tpe)
      }
    }

  override def compileDeepCopy(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC](compileDeepCopyImpl)

  private def compilePrintImpl(using Quotes)(using ctx: RecordDeclTC, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Option[?]] =>
          CCallExpr(getOptionPrinter(tpe).ref, List(expr))
      }
    }

  override def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = ensureCtx[RecordDeclTC](compilePrintImpl)

  private val valField = "val"
  private val definedField = "defined"

  private val CREATE_NONE = "CREATE_NONE"
  private val CREATE_SOME = "CREATE_SOME"
  private val GET_OR_ELSE = "GET_OR_ELSE"
  private val EQUALS = "EQUALS"
  private val PRINT = "PRINT"

  private def getNoneCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE_NONE, buildNoneCreator(tpe))
  }

  private def getSomeCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> CREATE_SOME, buildSomeCreator(tpe))
  }

  private def buildNoneCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    val recordDecl = getRecordDecl(tpe)
    val name = "createNone_" + recordDecl.name

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

  private def buildSomeCreator(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    val typeArgs(List(wrappedType)) = tpe.widen
    val recordDecl = getRecordDecl(tpe)
    val name = "createSome_" + recordDecl.name

    val valParam = CParmVarDecl("val", recordDecl.getField(valField).declaredType)

    val optDecl = CVarDecl(
      "opt",
      recordDecl.getTypeForDecl,
      Some(CDesignatedInitExpr(
        List(
          valField -> retain(valParam.ref, wrappedType),
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

  private def getGetOrElse(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> GET_OR_ELSE, buildGetOrElse(getRecordDecl(tpe)))
  }

  private def buildGetOrElse(recordDecl: CRecordDecl)(using ctx: TranslationContext): CFunctionDecl = {
    val name = "getOrElse_" + recordDecl.name

    val optParam = CParmVarDecl("opt", recordDecl.getTypeForDecl)
    val valType = recordDecl.getField(valField).declaredType
    val defaultValParam = CParmVarDecl("defaultVal", valType)

    val body = CCompoundStmt(List(
      CConditionalOperator(
        CMemberExpr(optParam.ref, definedField),
        CMemberExpr(optParam.ref, valField),
        defaultValParam.ref
      )
    ))

    CFunctionDecl(name, List(optParam, defaultValParam), valType, Some(body))
  }

  private def getOptionEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> EQUALS, buildOptionEquals(tpe))
  }

  private def buildOptionEquals(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(wrappedType)) = tpe.widen

    val name = "equals_" + recordDecl.name

    val paramLeft = CParmVarDecl("left", recordDecl.getTypeForDecl)
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
        cascade.dispatch(_.compileEquals)(
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

  private def getOptionDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> DEEP_COPY, buildOptionDeepCopy(tpe))
  }

  private def buildOptionDeepCopy(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(wrappedType)) = tpe.widen

    val name = "deepCopy_" + recordDecl.name

    val optParam = CParmVarDecl("opt", recordDecl.getTypeForDecl)

    val body = CCompoundStmt(List(
      CIfStmt(
        CMemberExpr(optParam.ref, definedField),
        CReturnStmt(Some(CCallExpr(
          getSomeCreator(tpe).ref,
          List(CompileDataStructure.deepCopy(
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

  private def getOptionPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    ctx.recordFunMap.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe) -> PRINT, buildOptionPrinter(tpe))
  }

  private def buildOptionPrinter(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CFunctionDecl = {
    import quotes.reflect.*

    val recordDecl = getRecordDecl(tpe)
    val typeArgs(List(wrappedType)) = tpe.widen

    val name = "print_" + recordDecl.name

    val optParam = CParmVarDecl("opt", recordDecl.getTypeForDecl)

    val body = CCompoundStmt(List(
      CIfStmt(
        CMemberExpr(optParam.ref, definedField),
        CCompoundStmt(List(
          CompileString.printf("Some("),
          cascade.dispatch(_.compilePrint)(
            CMemberExpr(optParam.ref, valField),
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
