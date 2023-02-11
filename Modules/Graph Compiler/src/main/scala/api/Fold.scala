package api

import clangast.WithContext
import clangast.decl.CFunctionDecl
import clangast.expr.CExpr
import clangast.types.CType
import compiler.MacroCompiler

import scala.quoted.*

case class Fold[V](init: WithContext[CExpr], cType: WithContext[CType], lines: List[FLine[_, V]]) extends Event[V] {
  override def inputs: List[ReSource] = lines.map(_.input)

  override val baseName: String = "fold"
}

object Fold {
  class FoldFactory[V, R](input: Event[V]) {
    inline def apply[C <: MacroCompiler](inline init: R)(inline f: (R, V) => R)(using mc: C): Fold[R] =
      Fold(
        mc.compileExpr(init),
        mc.compileType[R],
        List(FLine(input, mc.compileAnonFun(f)))
      )
  }
}

extension [V](input: Event[V])
  inline def fold[R]: Fold.FoldFactory[V, R] = new Fold.FoldFactory(input)
