package lore.dsl

import reactives.core.CreationTicket
import reactives.default.*

import scala.quoted.*

trait Invariant {

  val representation: String

}

def showInvariantCode(expr: Expr[() => Boolean])(using Quotes): Expr[String] = {
  Expr(expr.show)
}

case class DefaultInvariant(override val representation: String) extends Invariant

def constructInvariant(pred: Expr[() => Boolean])(using Quotes): Expr[Invariant] =
  '{ DefaultInvariant(${ showInvariantCode(pred) }) }

object Invariant {
  private inline def createInvariant(inline pred: () => Boolean): Invariant =
    ${ constructInvariant('{ pred }) }

  inline def apply(inline pred: Boolean): Invariant = {
    val x: Signal[Boolean] = Signal {
      pred
    }

    val invariant = createInvariant { () => pred }

    x.observe { it =>
      if !it then {
        throw new IllegalStateException(s"Violated Invariant ${invariant.representation}")
      }
    }

    invariant
  }
}
