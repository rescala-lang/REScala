package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CLeftShiftAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "<<="

  override def toExpr(using Quotes): Expr[CLeftShiftAssignmentExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CLeftShiftAssignmentExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CLeftShiftAssignmentExpr =
    CLeftShiftAssignmentExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
