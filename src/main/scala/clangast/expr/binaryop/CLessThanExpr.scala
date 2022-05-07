package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CLessThanExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "<"

  override def toExpr(using Quotes): Expr[CLessThanExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CLessThanExpr($lhsExpr, $rhsExpr) }
  }
}
