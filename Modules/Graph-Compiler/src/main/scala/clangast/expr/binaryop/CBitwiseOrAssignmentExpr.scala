package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CBitwiseOrAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "|="

  override def toExpr(using Quotes): Expr[CBitwiseOrAssignmentExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CBitwiseOrAssignmentExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CBitwiseOrAssignmentExpr =
    CBitwiseOrAssignmentExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
