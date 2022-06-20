package compiler.base

import clangast.given
import clangast.decl.*
import clangast.stmt.*
import compiler.context.{FunctionDeclTC, TranslationContext, ValueDeclTC}
import compiler.CompilerCascade
import compiler.base.CompileDataStructure.retain

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
  
      val params = defDef.termParamss.flatMap(_.params)
  
      val compiledParams = params.map(cascade.dispatch(_.compileValDefToCParmVarDecl))
  
      val body = rhs.map { (t: Term) =>
        t match
          case block: Block => cascade.dispatch(_.compileBlockToFunctionBody)(block)
          case Return(expr, _) => CCompoundStmt(List(CReturnStmt(Some(cascade.dispatch(_.compileTermToCExpr)(expr)))))
          case term if term.tpe =:= TypeRepr.of[Unit] => CCompoundStmt(List(cascade.dispatch(_.compileTermToCStmt)(term)))
          case term => CCompoundStmt(List(CReturnStmt(Some(cascade.dispatch(_.compileTermToCExpr)(term)))))
      }

      val pos = defDef.pos
      val inferredName = "anonfun_" + pos.sourceFile.name.stripSuffix(".scala") + "_" + (pos.startLine + 1) + "_" + (pos.startColumn + 1)
  
      val cname = if defName.equals("$anonfun") then inferredName else defName
  
      val decl = CFunctionDecl(cname, compiledParams, cascade.dispatch(_.compileTypeRepr)(returnTpt.tpe), body)
  
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
