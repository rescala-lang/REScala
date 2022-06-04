package compiler.base

import clangast.given
import clangast.decl.{CDecl, CFunctionDecl, CParmVarDecl, CVarDecl}
import clangast.stmt.{CCompoundStmt, CReturnStmt}
import compiler.{CompilerCascade, PartialCompiler, TranslationContext}

import scala.quoted.*

object CompileDefinition extends PartialCompiler {
  override def compileDefinition(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Definition, CDecl] = {
      import quotes.reflect.*
    
      {
        case defDef: DefDef => cascade.compileDefDef(defDef)
        case valDef: ValDef => cascade.compileValDefToCVarDecl(valDef)
      }
    }

  override def compileDefDef(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.DefDef, CFunctionDecl] = defDef => {
      import quotes.reflect.*
  
      val DefDef(defName, _, returnTpt, rhs) = defDef
  
      val params = defDef.termParamss.flatMap(_.params)
  
      val compiledParams = params.map(cascade.compileValDefToCParmVarDecl)
  
      val body = rhs.map { (t: Term) =>
        t match
          case block: Block => cascade.compileBlockToFunctionBody(block)
          case Return(expr, _) => CCompoundStmt(List(CReturnStmt(Some(cascade.compileTermToCExpr(expr)))))
          case term => CCompoundStmt(List(CReturnStmt(Some(cascade.compileTermToCExpr(term)))))
      }
  
      val inferredName = try {
        defDef.symbol.owner.owner.name
      } catch {
        case _: Exception => defName
      }
  
      val cname = if defName.equals("$anonfun") then inferredName else defName
  
      val decl = CFunctionDecl(cname, compiledParams, cascade.compileTypeRepr(returnTpt.tpe), body)
  
      ctx.nameToFunctionDecl.put(cname, decl)
  
      decl
    }

  override def compileValDefToCVarDecl(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = {
      import quotes.reflect.*
    
      {
        case ValDef(name, tpt, rhs) =>
          val decl = CVarDecl(name, cascade.compileTypeRepr(tpt.tpe), rhs.map(cascade.compileTermToCExpr))

          ctx.nameToDecl.put(name, decl)

          decl
      }
    }

  override def compileValDefToCParmVarDecl(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CParmVarDecl] = {
      import quotes.reflect.*
    
      {
        case ValDef(name, tpt, _) =>
          val decl = CParmVarDecl(name, cascade.compileTypeRepr(tpt.tpe))

          ctx.nameToDecl.put(name, decl)

          decl
      }
    }
}
