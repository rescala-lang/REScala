package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CBitwiseOrExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "|"

  override def toExpr(using Quotes): Expr[CBitwiseOrExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CBitwiseOrExpr($lhsExpr, $rhsExpr) }
  }
}
