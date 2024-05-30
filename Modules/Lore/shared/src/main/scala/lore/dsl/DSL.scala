package lore.dsl

import reactives.core.{AdmissionTicket, ReSource, StaticTicket}
import reactives.default.*
import reactives.operator.Interface.State as BundleState

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
