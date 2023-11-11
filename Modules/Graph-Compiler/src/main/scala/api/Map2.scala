package api

import clangast.WithContext
import clangast.decl.CFunctionDecl
import clangast.types.CType
import compiler.MacroCompiler

case class Map2[A, B, R](l: Event[A], r: Event[B], cType: WithContext[CType], f: WithContext[CFunctionDecl])
    extends Event[R] {
  override def inputs: List[ReSource] = List(l, r)

  override val baseName: String = "map2"
}

object Map2 {
  class Map2Factory[A, B, R](l: Event[A]) {
    inline def apply[C <: MacroCompiler](inline r: Event[B])(inline f: (A, B) => R)(using mc: C): Map2[A, B, R] =
      Map2(
        l,
        r,
        mc.compileType[Option[R]],
        mc.compileAnonFun(f)
      )
  }
}

extension [A](l: Event[A])
  inline def map2[B, R]: Map2.Map2Factory[A, B, R] = new Map2.Map2Factory(l)
