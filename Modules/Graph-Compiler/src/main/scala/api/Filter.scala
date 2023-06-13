package api

import clangast.WithContext
import clangast.decl.CFunctionDecl
import clangast.types.CType
import compiler.MacroCompiler


case class Filter[V](input: Event[V], cType: WithContext[CType], f: WithContext[CFunctionDecl]) extends Event[V] {
  override def inputs: List[ReSource] = List(input)

  override val baseName: String = "filter"
}

object Filter {
  class FilterFactory[V](input: Event[V]) {
    inline def apply[C <: MacroCompiler](inline f: V => Boolean)(using mc: C): Filter[V] =
      Filter(
        input,
        mc.compileType[Option[V]],
        mc.compileAnonFun(f)
      )
  }
}

extension [V](input: Event[V])
  inline def filter: Filter.FilterFactory[V] = new Filter.FilterFactory(input)
