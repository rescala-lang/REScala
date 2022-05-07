package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CProdExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "*"

  override def toExpr(using Quotes): Expr[CProdExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CProdExpr($lhsExpr, $rhsExpr) }
  }
}
