package api

import clangast.WithContext
import clangast.decl.CFunctionDecl
import compiler.MacroCompiler

import scala.quoted.*

case class Filter[V](input: Event[V], f: WithContext[CFunctionDecl]) extends Event[V] {
  override def inputs: List[ReSource] = List(input)

  override val baseName: String = "filter"
}

object Filter {
  class FilterFactory[V](input: Event[V]) {
    inline def apply[C <: MacroCompiler](inline funName: String = "filter")(inline f: V => Boolean)(using mc: C): Filter[V] =
      Filter(
        input,
        mc.compileAnonFun(f, funName)
      )
  }
}

extension [V] (input: Event[V])
  inline def filter: Filter.FilterFactory[V] = new Filter.FilterFactory(input)
