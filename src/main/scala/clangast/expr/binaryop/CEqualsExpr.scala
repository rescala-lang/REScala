package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CEqualsExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "=="

  override def toExpr(using Quotes): Expr[CEqualsExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CEqualsExpr($lhsExpr, $rhsExpr) }
  }
}
