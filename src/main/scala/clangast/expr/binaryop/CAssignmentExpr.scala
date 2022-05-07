package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "="

  override def toExpr(using Quotes): Expr[CAssignmentExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CAssignmentExpr($lhsExpr, $rhsExpr) }
  }
}
