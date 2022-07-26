package compiler.context

import api2.CompiledReactive
import clangast.decl.CParmVarDecl

import scala.collection.mutable

trait ReactiveTC extends TranslationContext {
  val inputParameters: MappingLabel[String, CParmVarDecl] = new MappingLabel()

  protected val reactives: mutable.ListBuffer[CompiledReactive] = new mutable.ListBuffer()

  def addReactive(r: CompiledReactive): Unit = reactives.append(r)

  def reactivesList: List[CompiledReactive] = reactives.toList
}
