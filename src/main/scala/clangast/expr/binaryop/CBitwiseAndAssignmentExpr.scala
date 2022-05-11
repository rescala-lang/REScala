package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CBitwiseAndAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "&="

  override def toExpr(using Quotes): Expr[CBitwiseAndAssignmentExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CBitwiseAndAssignmentExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CBitwiseAndAssignmentExpr =
    CBitwiseAndAssignmentExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
