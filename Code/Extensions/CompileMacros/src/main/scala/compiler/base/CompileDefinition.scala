package compiler.base

import clangast.given
import clangast.decl.*
import clangast.expr.{CFalseLiteral, CTrueLiteral}
import clangast.stmt.*
import compiler.context.{FunctionDeclTC, TranslationContext, ValueDeclTC}
import compiler.CompilerCascade
import compiler.base.CompileDataStructure.{release, retain}

import scala.quoted.*

object CompileDefinition extends DefinitionPC {
  override def compileDefinition(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Definition, CDecl] = {
      import quotes.reflect.*
    
      {
        case defDef: DefDef => cascade.dispatch(_.compileDefDef)(defDef)
        case valDef: ValDef => cascade.dispatch(_.compileValDefToCVarDecl)(valDef)
      }
    }

  private def compileDefDefImpl(using Quotes)(using ctx: FunctionDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.DefDef, CFunctionDecl] = defDef => {
      import quotes.reflect.*
  
      val DefDef(defName, _, returnTpt, rhs) = defDef
  
      val params: List[ValDef] = defDef.termParamss.flatMap(_.params)
  
      val compiledParams: List[CParmVarDecl] = params.map(cascade.dispatch(_.compileValDefToCParmVarDecl))
      compiledParams.foreach(p => ctx.registerValueName(p.name))

      val returnType = cascade.dispatch(_.compileTypeRepr)(returnTpt.tpe)

      val (retainParams, releaseParams): (List[CStmt], List[CStmt]) = (params zip compiledParams).collect {
        case (p, cp) if cascade.dispatch(_.usesRefCount)(p.tpt.tpe) =>
          (CExprStmt(retain(cp.ref, p.tpt.tpe)), release(cp.ref, p.tpt.tpe, CFalseLiteral).get)
      }.unzip
  
      val plainBody: Option[CCompoundStmt] = rhs.map { (t: Term) =>
        t match
          case block: Block => cascade.dispatch(_.compileBlockToFunctionBody)(block)
          case Return(expr, _) => CCompoundStmt(List(CReturnStmt(Some(cascade.dispatch(_.compileTermToCExpr)(expr)))))
          case term if term.tpe =:= TypeRepr.of[Unit] => CCompoundStmt(List(cascade.dispatch(_.compileTermToCStmt)(term)))
          case _ => CCompoundStmt(List(CReturnStmt(Some(cascade.dispatch(_.compileTermToCExpr)(t)))))
      }

      val body = plainBody.map { ccStmt =>
        val releaseLocalVars = CompileDataStructure.releaseLocalVars(ccStmt.body)
        val resUsesRefCount = cascade.dispatch(_.usesRefCount)(returnTpt.tpe)

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
      val inferredName = "anonfun_" + pos.sourceFile.name.stripSuffix(".scala") + "_" + (pos.startLine + 1) + "_" + (pos.startColumn + 1)
  
      val cname = if defName.equals("$anonfun") then inferredName else defName
  
      val decl = CFunctionDecl(cname, compiledParams, returnType, body)
  
      ctx.nameToFunctionDecl.put(cname, decl)
  
      decl
    }

  override def compileDefDef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.DefDef, CFunctionDecl] = ensureCtx[FunctionDeclTC](compileDefDefImpl)

  private def compileValDefToCVarDeclImpl(using Quotes)(using ctx: ValueDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = {
      import quotes.reflect.*
    
      {
        case ValDef(name, tpt, rhs) =>
          val init = rhs.map(cascade.dispatch(_.compileTermToCExpr)).map(retain(_, tpt.tpe))

          // Make sure that the release function for this type is in context when it has to be released
          cascade.dispatchLifted(_.compileRelease)(tpt.tpe)

          ctx.registerValueName(name)
          val decl = CVarDecl(name, cascade.dispatch(_.compileTypeRepr)(tpt.tpe), init)

          ctx.nameToDecl.put(name, decl)

          decl
      }
    }

  override def compileValDefToCVarDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = ensureCtx[ValueDeclTC](compileValDefToCVarDeclImpl)

  private def compileValDefToCParmVarDeclImpl(using Quotes)(using ctx: ValueDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CParmVarDecl] = {
      import quotes.reflect.*
    
      {
        case ValDef(name, tpt, _) =>
          val decl = CParmVarDecl(name, cascade.dispatch(_.compileTypeRepr)(tpt.tpe))

          ctx.nameToDecl.put(name, decl)

          decl
      }
    }

  override def compileValDefToCParmVarDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CParmVarDecl] = ensureCtx[ValueDeclTC](compileValDefToCParmVarDeclImpl)
}
