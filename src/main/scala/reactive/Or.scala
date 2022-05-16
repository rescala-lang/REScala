package reactive

import clangast.types.CType
import macros.ScalaToC

import scala.annotation.targetName
import scala.quoted.*

case class Or[V](left: Event[V], right: Event[V], cType: CType) extends Event[V] {
  override def inputs: List[ReSource] = List(left, right)

  override val baseName: String = "or"
}

object Or {
  def orCode[V](left: Expr[Event[V]], right: Expr[Event[V]])(using Quotes, Type[V]): Expr[Or[V]] = {
    import quotes.reflect.*

    val tpeCAST = ScalaToC.compileType[V]

    '{ Or($left, $right, $tpeCAST) }
  }
}

extension [V] (inline left: Event[V])
  @targetName("or")
  inline def ||(inline right: Event[V]): Or[V] = ${ Or.orCode('left, 'right) }