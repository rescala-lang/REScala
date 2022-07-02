package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CMinusAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "-="

  override def toExpr(using Quotes): Expr[CMinusAssignmentExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CMinusAssignmentExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CMinusAssignmentExpr =
    CMinusAssignmentExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
