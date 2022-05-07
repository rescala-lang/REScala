package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CGreaterEqualsExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = ">="

  override def toExpr(using Quotes): Expr[CGreaterEqualsExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CGreaterEqualsExpr($lhsExpr, $rhsExpr) }
  }
}
