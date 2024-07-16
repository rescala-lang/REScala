package lore.dsl

import reactives.SelectedScheduler.State as BundleState
import reactives.core.{AdmissionTicket, ReSource, StaticTicket}
import reactives.default.*

import scala.quoted.*

case class Requires[S, A](
    inputs: List[ReSource.of[BundleState]],
    fun: StaticTicket[BundleState] => (S, A) => Boolean,
    representation: String
)

case class Ensures[S, A](
    inputs: List[ReSource.of[BundleState]],
    fun: StaticTicket[BundleState] => (S, A) => Boolean,
    representation: String
)

def showPredicateCode(expr: Expr[(?, ?) => Boolean])(using Quotes): Expr[String] = {
  Expr(expr.show)
}
