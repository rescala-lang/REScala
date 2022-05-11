package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CRightShiftAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = ">>="

  override def toExpr(using Quotes): Expr[CRightShiftAssignmentExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CRightShiftAssignmentExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CRightShiftAssignmentExpr =
    CRightShiftAssignmentExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
