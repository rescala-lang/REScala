package macros

import clangast.given
import clangast.decl.{CDecl, CFunctionDecl, CParmVarDecl, CVarDecl}
import clangast.stmt.{CCompoundStmt, CReturnStmt}
import macros.ScalaToC.*
import macros.CompileTerm.*
import macros.CompileType.*

import scala.quoted.*

object CompileDefinition {
  def compileDefinition(using Quotes)(definition: quotes.reflect.Definition, ctx: TranslationContext): CDecl = {
    import quotes.reflect.*

    definition match {
      case defDef: DefDef => compileDefDef(defDef, ctx)
      case valDef: ValDef => compileValDefToCVarDecl(valDef, ctx)
      case _ => throw new MatchError(definition.show(using Printer.TreeStructure))
    }
  }

  def compileDefDef(using Quotes)(defDef: quotes.reflect.DefDef, ctx: TranslationContext): CFunctionDecl = {
    import quotes.reflect.*

    val DefDef(name, _, returnTpt, rhs) = defDef

    val params = defDef.termParamss.flatMap(_.params)

    val compiledParams = params.map(compileValDefToCParmVarDecl(_, ctx))

    val body = rhs.map { (t: Term) => t match
      case block: Block => compileBlockToFunctionBody(block, ctx)
      case Return(expr, _) => CCompoundStmt(List(CReturnStmt(Some(compileTermToCExpr(expr, ctx)))))
      case term => CCompoundStmt(List(CReturnStmt(Some(compileTermToCExpr(term, ctx)))))
    }

    val decl = CFunctionDecl(name, compiledParams, compileTypeRepr(returnTpt.tpe, ctx), body)

    ctx.nameToDecl.put(name, decl)

    decl
  }

  def compileValDefToCVarDecl(using Quotes)(valDef: quotes.reflect.ValDef, ctx: TranslationContext): CVarDecl = {
    import quotes.reflect.*

    val ValDef(name, tpt, rhs) = valDef

    val decl = CVarDecl(name, compileTypeRepr(tpt.tpe, ctx), rhs.map(compileTermToCExpr(_, ctx)))

    ctx.nameToDecl.put(name, decl)

    decl
  }

  def compileValDefToCParmVarDecl(using Quotes)(valDef: quotes.reflect.ValDef, ctx: TranslationContext): CParmVarDecl = {
    import quotes.reflect.*

    val ValDef(name, tpt, _) = valDef

    val decl = CParmVarDecl(name, compileTypeRepr(tpt.tpe, ctx))

    ctx.nameToDecl.put(name, decl)

    decl
  }
}
