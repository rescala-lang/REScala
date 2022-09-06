package compiler

import api2.{CompiledReactive, CompiledSignalExpr, GraphCompiler}
import clangast.*
import clangast.given
import clangast.WithContext
import clangast.decl.CFunctionDecl
import clangast.expr.CIntegerLiteral
import clangast.stmt.{CCompoundStmt, CReturnStmt}
import compiler.base.*
import compiler.ext.*
import compiler.context.ReactiveTC

import scala.quoted.*

trait ReactiveMacroCompilerCode extends MacroCompilerCode {
  protected type CTX <: ReactiveTC

  def compileGraphCode(appName: Expr[String])(graph: Expr[_])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()

    val Inlined(_, _, Inlined(_, _, Block(stmts, expr))) = graph.asTerm: @unchecked

    stmts.foreach(cascade.dispatch(_.compileReactiveTopLevelStmt))

    val outputReactives = expr match {
      case Apply(TypeApply(Select(_, "apply"), _), l) =>
        l.collect { case i: Ident => ctx.reactivesList.find(_.name.equals(i.name)) }.flatten
    }

    val gc = new GraphCompiler(using summon[Quotes])(ctx.reactivesList, outputReactives, appName.valueOrAbort)
    gc.writeIntoDir("out/" + appName.valueOrAbort, "gcc")

    '{()}
  }
}
