package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CGreaterThanExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = ">"

  override def toExpr(using Quotes): Expr[CGreaterThanExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CGreaterThanExpr($lhsExpr, $rhsExpr) }
  }
}
