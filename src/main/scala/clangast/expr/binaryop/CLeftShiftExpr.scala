package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CLeftShiftExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "<<"

  override def toExpr(using Quotes): Expr[CLeftShiftExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CLeftShiftExpr($lhsExpr, $rhsExpr) }
  }
}
