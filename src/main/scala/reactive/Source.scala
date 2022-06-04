package reactive

import clangast.WithContext
import clangast.types.CType
import compiler.MacroCompiler

import scala.quoted.*

case class Source[V](name: String, cType: WithContext[CType]) extends Event[V] {
  override def inputs: List[ReSource] = Nil

  override val baseName: String = "source"

  override def valueName: String = name
  
  val validName: String = valueName + "_valid"
}

object Source {
  class SourceFactory[V] {
    inline def apply[C <: MacroCompiler](inline name: String)(using mc: C): Source[V] =
      new Source[V](name, mc.compileType[V])
  }

  inline def apply[V]: SourceFactory[V] = new SourceFactory
}
