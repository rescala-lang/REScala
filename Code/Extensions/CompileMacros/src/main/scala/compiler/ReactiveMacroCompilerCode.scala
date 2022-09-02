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

  def compileReactiveExprCode(e: Expr[_])(using Quotes): Expr[WithContext[CFunctionDecl]] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()

    val compiledBlock = e.asTerm match {
      case block: Block =>
        cascade.dispatch(_.compileBlockToFunctionBody)(block) match {
          case block: CCompoundStmt => block
        }
      case Inlined(_, _, term) =>
        val compiledTerm = cascade.dispatch(_.compileTermToCExpr)(term)
        CCompoundStmt(List(CReturnStmt(Some(compiledTerm))))
    }

    val pos = Position.ofMacroExpansion
    val inferredName =
      "anonfun_" + pos.sourceFile.name.stripSuffix(".scala") + "_" + (pos.startLine + 1) + "_" + (pos.startColumn + 1)

    val funDecl = CFunctionDecl(
      inferredName,
      ctx.inputParameters.values.toList,
      cascade.dispatch(_.compileTypeRepr)(e.asTerm.tpe),
      Some(compiledBlock)
    )

    WithContext(funDecl, ctx).toExpr
  }

  def compileGraphCode(graph: Expr[_])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()

    val Inlined(_, _, Inlined(_, _, Block(stmts, expr))) = graph.asTerm

    stmts.foreach(cascade.dispatch(_.compileReactiveTopLevelStmt))

    val outputReactives = expr match {
      case Apply(TypeApply(Select(_, "apply"), _), l) =>
        l.collect { case i: Ident => ctx.reactivesList.find(_.name.equals(i.name)) }.flatten
    }

    val gc = new GraphCompiler(using summon[Quotes])(ctx.reactivesList, outputReactives, "metaBundleTest")
    gc.writeIntoDir("out", "gcc")

    '{()}
  }
}
