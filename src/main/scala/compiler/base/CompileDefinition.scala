package compiler.base

import clangast.given
import clangast.decl.*
import clangast.stmt.*
import compiler.context.{FunctionDeclTC, TranslationContext, ValueDeclTC}
import compiler.CompilerCascade

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

  def compileDefDef(using Quotes)(using ctx: FunctionDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.DefDef, CFunctionDecl] = defDef => {
      import quotes.reflect.*
  
      val DefDef(defName, _, returnTpt, rhs) = defDef
  
      val params = defDef.termParamss.flatMap(_.params)
  
      val compiledParams = params.map(cascade.dispatch(_.compileValDefToCParmVarDecl))
  
      val body = rhs.map { (t: Term) =>
        t match
          case block: Block => cascade.dispatch(_.compileBlockToFunctionBody)(block)
          case Return(expr, _) => CCompoundStmt(List(CReturnStmt(Some(cascade.dispatch(_.compileTermToCExpr)(expr)))))
          case term if term.tpe =:= TypeRepr.of[Unit] => CCompoundStmt(List(CExprStmt(cascade.dispatch(_.compileTermToCExpr)(term))))
          case term => CCompoundStmt(List(CReturnStmt(Some(cascade.dispatch(_.compileTermToCExpr)(term)))))
      }
  
      val inferredName = try {
        Symbol.spliceOwner.owner.name
      } catch {
        case _: Exception => defName
      }
  
      val cname = if defName.equals("$anonfun") then inferredName else defName
  
      val decl = CFunctionDecl(cname, compiledParams, cascade.dispatch(_.compileTypeRepr)(returnTpt.tpe), body)
  
      ctx.nameToFunctionDecl.put(cname, decl)
  
      decl
    }

  override def compileDefDef(using q: Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.DefDef, CFunctionDecl] = ctx match {
      case c: FunctionDeclTC => compileDefDef(using q)(using c, cascade)
      case _ => PartialFunction.empty
    }

  def compileValDefToCVarDecl(using Quotes)(using ctx: ValueDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = {
      import quotes.reflect.*
    
      {
        case ValDef(name, tpt, rhs) =>
          val decl = CVarDecl(name, cascade.dispatch(_.compileTypeRepr)(tpt.tpe), rhs.map(cascade.dispatch(_.compileTermToCExpr)))

          ctx.nameToDecl.put(name, decl)

          decl
      }
    }

  override def compileValDefToCVarDecl(using q: Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = ctx match {
      case c: ValueDeclTC => compileValDefToCVarDecl(using q)(using c, cascade)
      case _ => PartialFunction.empty
    }

  def compileValDefToCParmVarDecl(using Quotes)(using ctx: ValueDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CParmVarDecl] = {
      import quotes.reflect.*
    
      {
        case ValDef(name, tpt, _) =>
          val decl = CParmVarDecl(name, cascade.dispatch(_.compileTypeRepr)(tpt.tpe))

          ctx.nameToDecl.put(name, decl)

          decl
      }
    }

  override def compileValDefToCParmVarDecl(using q: Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CParmVarDecl] = ctx match {
      case c: ValueDeclTC => compileValDefToCParmVarDecl(using q)(using c, cascade)
      case _ => PartialFunction.empty
    }
}
