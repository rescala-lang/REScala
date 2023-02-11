package api

import clangast.WithContext
import clangast.types.CType
import compiler.MacroCompiler

import scala.annotation.targetName
import scala.quoted.*

case class Or[V](left: Event[V], right: Event[V], cType: WithContext[CType]) extends Event[V] {
  override def inputs: List[ReSource] = List(left, right)

  override val baseName: String = "or"
}

object Or {
  class OrFactory[V](left: Event[V]) {
    inline def apply[C <: MacroCompiler](inline right: Event[V])(using mc: C): Or[V] =
      Or(
        left,
        right,
        mc.compileType[Option[V]]
      )
  }
}

extension [V](left: Event[V])
  @targetName("or")
  inline def || : Or.OrFactory[V] = new Or.OrFactory(left)
