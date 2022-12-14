package api

import clangast.WithContext
import clangast.types.CType
import compiler.MacroCompiler

import scala.annotation.{compileTimeOnly, targetName}
import scala.quoted.*

case class Source[V](name: String, cType: WithContext[CType]) extends Event[V] {
  override def inputs: List[ReSource] = Nil

  override val baseName: String = "source"

  override def valueName: String = name

  val validName: String = valueName + "_valid"

  @compileTimeOnly("This method can only be used in the parameters of CMainFunction.startTransaction")
  @targetName("assign")
  def :=(v: V): Unit = ???
}

object Source {
  inline def apply[V, C <: MacroCompiler]()(using mc: C): Source[V] =
    new Source[V](mc.valName, mc.compileType[Option[V]])
}
