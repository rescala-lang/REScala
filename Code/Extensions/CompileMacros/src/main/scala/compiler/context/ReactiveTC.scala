package compiler.context

import api2.{CompiledEvent, CompiledReactive}
import clangast.*
import clangast.given
import clangast.decl.{CFunctionDecl, CParmVarDecl}
import clangast.types.CVoidType

import scala.collection.mutable
import scala.quoted.*

trait ReactiveTC extends TranslationContext {
  val inputParameters: MappingLabel[String, (CParmVarDecl, Object)] = new MappingLabel()

  protected val reactives: mutable.ListBuffer[CompiledReactive] = new mutable.ListBuffer()

  def addReactive(r: CompiledReactive): Unit = reactives.append(r)

  def reactivesList: List[CompiledReactive] = reactives.toList

  def createMissingSources(using Quotes)(): Unit =
    inputParameters.values.foreach {
      case (p, tpe) =>
        if !reactives.exists(_.name.equals(p.name)) then
          addReactive(CompiledEvent(p.name, CFunctionDecl("", List(), p.declaredType), tpe.asInstanceOf[quotes.reflect.TypeRepr]))
    }
}
