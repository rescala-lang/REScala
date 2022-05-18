package reactive

import clangast.WithContext
import clangast.decl.CFunctionDecl
import macros.ScalaToC

import scala.quoted.*

case class Filter[V](input: Event[V], f: WithContext[CFunctionDecl]) extends Event[V] {
  override def inputs: List[ReSource] = List(input)

  override val baseName: String = "filter"
}

object Filter {
  def filterCode[V](input: Expr[Event[V]], f: Expr[V => Boolean], funName: Expr[String])(using Quotes, Type[V]): Expr[Filter[V]] = {
    import quotes.reflect.*
    
    val cast = ScalaToC.compileAnonFun(f, funName)
    
    '{ Filter($input, $cast) }
  }
}

extension [V] (inline input: Event[V]) inline def filter(inline funName: String = "filter")(inline f: V => Boolean): Filter[V] =
  ${ Filter.filterCode('input, 'f, 'funName) }
