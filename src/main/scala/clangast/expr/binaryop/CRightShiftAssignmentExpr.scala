package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CRightShiftAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = ">>="

  override def toExpr(using Quotes): Expr[CRightShiftAssignmentExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CRightShiftAssignmentExpr($lhsExpr, $rhsExpr) }
  }
}
