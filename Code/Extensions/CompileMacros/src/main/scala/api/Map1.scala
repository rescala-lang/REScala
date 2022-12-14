package api

import clangast.WithContext
import clangast.decl.CFunctionDecl
import clangast.traversal.CASTMapper
import clangast.types.{CType, CVoidType}
import compiler.MacroCompiler
import compiler.debug.Debug

import scala.quoted.*

case class Map1[A, R](input: Event[A], cType: WithContext[CType], f: WithContext[CFunctionDecl]) extends Event[R] {
  override def inputs: List[ReSource] = List(input)

  override val baseName: String = "map"
}

object Map1 {
  class Map1Factory[A, R](input: Event[A]) {
    inline def apply[C <: MacroCompiler](inline f: A => R)(using mc: C): Map1[A, R] =
      Map1(
        input,
        mc.compileType[Option[R]],
        mc.compileAnonFun(f)
      )
  }
}

extension [A](input: Event[A])
  inline def map[R]: Map1.Map1Factory[A, R] = new Map1.Map1Factory(input)

  inline def observe[C <: MacroCompiler](inline f: A => Unit)(using mc: C): Map1[A, Unit] =
    Map1(
      input,
      WithContext(CVoidType, Nil, Nil, Nil),
      mc.compileAnonFun(f)
    )
