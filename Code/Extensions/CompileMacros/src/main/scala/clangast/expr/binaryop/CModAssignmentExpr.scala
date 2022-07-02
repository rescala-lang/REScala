package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CModAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "%="

  override def toExpr(using Quotes): Expr[CModAssignmentExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CModAssignmentExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CModAssignmentExpr =
    CModAssignmentExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
