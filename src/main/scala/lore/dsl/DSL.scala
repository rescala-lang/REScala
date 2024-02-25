package lore.dsl

import rescala.default.*

import scala.quoted.*

type Source[A] = Var[A]
type Derived[A] = Signal[A]

object Source {
  inline def apply[A](inline expr: A): Var[A] = Var(expr)
}

object Derived {
  inline def apply[A](inline expr: A): Signal[A] = Signal {
    expr
  }
}

case class Requires[S, A](predicate: (S, A) => Boolean, representation: String)

case class Ensures[S, A](predicate: (S, A) => Boolean, representation: String)

def showPredicateCode(expr: Expr[(_, _) => Boolean])(using Quotes): Expr[String] = {
  Expr(expr.show)
}
