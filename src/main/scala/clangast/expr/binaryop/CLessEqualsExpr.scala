package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CLessEqualsExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "<="

  override def toExpr(using Quotes): Expr[CLessEqualsExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CLessEqualsExpr($lhsExpr, $rhsExpr) }
  }
}
