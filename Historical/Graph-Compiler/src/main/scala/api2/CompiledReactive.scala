package api2

import clangast.decl.CFunctionDecl
import clangast.expr.CExpr
import clangast.types.CType

import scala.quoted.*

trait CompiledReactive {
  val name: String
  val updateFun: CFunctionDecl
  val tpr: Any
  def rename(newName: String): CompiledReactive
  def cType: CType                                    = updateFun.returnType.unqualType
  def inputs: List[String]                            = updateFun.parameters.map(_.name)
  def isSource: Boolean                               = updateFun.parameters.isEmpty
  def typeRepr(using Quotes): quotes.reflect.TypeRepr = tpr.asInstanceOf[quotes.reflect.TypeRepr]
}

trait CompiledSignal extends CompiledReactive

case class CompiledSignalExpr(
    name: String,
    updateFun: CFunctionDecl,
    tpr: Any
) extends CompiledSignal {
  override def rename(newName: String): CompiledSignalExpr = this.copy(name = newName)
}

case class CompiledFold(
    name: String,
    init: CExpr,
    primaryInput: String,
    updateFun: CFunctionDecl,
    tpr: Any
) extends CompiledSignal {
  override def rename(newName: String): CompiledFold = this.copy(name = newName)

  override def inputs: List[String] = primaryInput :: updateFun.parameters.drop(2).map(_.name)
}

case class CompiledEvent(name: String, updateFun: CFunctionDecl, tpr: Any, alwaysPropagates: Boolean = false)
    extends CompiledReactive {
  override def rename(newName: String): CompiledEvent = this.copy(name = newName)
}
