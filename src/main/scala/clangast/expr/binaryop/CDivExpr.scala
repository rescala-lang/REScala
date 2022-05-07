package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CDivExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "/"

  override def toExpr(using Quotes): Expr[CDivExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CDivExpr($lhsExpr, $rhsExpr) }
  }
}
