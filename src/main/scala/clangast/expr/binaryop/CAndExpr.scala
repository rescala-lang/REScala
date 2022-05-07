package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CAndExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "&&"

  override def toExpr(using Quotes): Expr[CAndExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CAndExpr($lhsExpr, $rhsExpr) }
  }
}
