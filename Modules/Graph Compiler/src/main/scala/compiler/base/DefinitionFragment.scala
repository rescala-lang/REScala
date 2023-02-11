package compiler.base

import clangast.given
import clangast.decl.*
import clangast.expr.{CFalseLiteral, CTrueLiteral}
import clangast.stmt.*
import compiler.context.{FunctionDeclTC, TranslationContext, ValueDeclTC}
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.base.DataStructureFragment.{release, retain}

import scala.quoted.*

object DefinitionFragment extends DefinitionIFFragment {
  override def compileDefinition(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Definition, CDecl] = {
    import quotes.reflect.*

    {
      case defDef: DefDef => dispatch[DefinitionIFFragment](_.compileDefDef)(defDef)
      case valDef: ValDef => dispatch[DefinitionIFFragment](_.compileValDefToCVarDecl)(valDef)
    }
  }

  override def compileDefDef(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.DefDef, CFunctionDecl] = ensureCtx[FunctionDeclTC] { ctx ?=> defDef =>
    import quotes.reflect.*

    val DefDef(defName, _, returnTpt, rhs) = defDef

    val params: List[ValDef] = defDef.termParamss.flatMap(_.params)

    val compiledParams: List[CParmVarDecl] = params.map(dispatch[DefinitionIFFragment](_.compileValDefToCParmVarDecl))
    compiledParams.foreach(p => ctx.registerValueName(p.name))

    val returnType = dispatch[TypeIFFragment](_.compileTypeRepr)(returnTpt.tpe)

    val (retainParams, releaseParams): (List[CStmt], List[CStmt]) = (params zip compiledParams).collect {
      case (p, cp) if dispatch[DataStructureIFFragment](_.usesRefCount)(p.tpt.tpe) =>
        (CExprStmt(retain(cp.ref, p.tpt.tpe)), release(cp.ref, p.tpt.tpe, CFalseLiteral).get)
    }.unzip

    val plainBody: Option[CCompoundStmt] = rhs.map { (t: Term) =>
      t match
        case block: Block => dispatch[TermIFFragment](_.compileBlockToFunctionBody)(block)
        case Return(expr, _) =>
          CCompoundStmt(List(CReturnStmt(Some(dispatch[TermIFFragment](_.compileTermToCExpr)(expr)))))
        case term if term.tpe =:= TypeRepr.of[Unit] =>
          CCompoundStmt(List(dispatch[TermIFFragment](_.compileTermToCStmt)(term)))
        case _ => CCompoundStmt(List(CReturnStmt(Some(dispatch[TermIFFragment](_.compileTermToCExpr)(t)))))
    }

    val body = plainBody.map { ccStmt =>
      val releaseLocalVars = DataStructureFragment.releaseLocalVars(ccStmt.body)
      val resUsesRefCount  = dispatch[DataStructureIFFragment](_.usesRefCount)(returnTpt.tpe)

      val (body, retainRes, releaseRes, returnRes) = ccStmt.body.last match {
        case CReturnStmt(Some(retVal)) if releaseParams.length + releaseLocalVars.length > 0 && resUsesRefCount =>
          val resName = ctx.uniqueValueName("_f_res")
          val resDecl = CVarDecl(resName, returnType, Some(retain(retVal, returnTpt.tpe)))
          (
            ccStmt.body.init,
            Some[CStmt](resDecl),
            release(resDecl.ref, returnTpt.tpe, CTrueLiteral),
            Some(CReturnStmt(Some(resDecl.ref)))
          )
        case ret: CReturnStmt =>
          (ccStmt.body.init, None, None, Some(ret))
        case _ =>
          (ccStmt.body, None, None, None)
      }

      CCompoundStmt(
        (if retainParams.isEmpty then Nil else retainParams :+ CEmptyStmt) ++
        body ++
        retainRes ++
        (if releaseLocalVars.isEmpty then Nil else CEmptyStmt :: releaseLocalVars) ++
        (if releaseParams.isEmpty then Nil else CEmptyStmt :: releaseParams) ++
        (if releaseRes.isEmpty then Nil else CEmptyStmt :: releaseRes.toList) ++
        returnRes
      )
    }

    val pos = defDef.pos
    val inferredName =
      "anonfun_" + pos.sourceFile.name.stripSuffix(".scala") + "_" + (pos.startLine + 1) + "_" + (pos.startColumn + 1)

    val cname = if defName.equals("$anonfun") then inferredName else defName

    CFunctionDecl(cname, compiledParams, returnType, body)
  }

  override def compileValDefToCVarDecl(using Quotes)(using fc: FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.ValDef, CVarDecl] = ensureCtx[ValueDeclTC] { ctx ?=>
    import quotes.reflect.*

    {
      case ValDef(name, tpt, rhs) =>
        val init = rhs.map(dispatch[TermIFFragment](_.compileTermToCExpr)).map(retain(_, tpt.tpe))

        // Make sure that the release function for this type is in context when it has to be released
        fc.dispatchLifted[DataStructureIFFragment](_.compileRelease)(tpt.tpe)

        ctx.registerValueName(name)
        val decl = CVarDecl(name, dispatch[TypeIFFragment](_.compileTypeRepr)(tpt.tpe), init)

        ctx.nameToDecl.put(name, decl)

        decl
    }
  }

  override def compileValDefToCParmVarDecl(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.ValDef, CParmVarDecl] = ensureCtx[ValueDeclTC] { ctx ?=>
    import quotes.reflect.*

    {
      case ValDef(name, tpt, _) =>
        val decl = CParmVarDecl(name, dispatch[TypeIFFragment](_.compileTypeRepr)(tpt.tpe))

        ctx.nameToDecl.put(name, decl)

        decl
    }
  }
}
