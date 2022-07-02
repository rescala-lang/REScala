package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CBitwiseXorAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "^="

  override def toExpr(using Quotes): Expr[CBitwiseXorAssignmentExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CBitwiseXorAssignmentExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CBitwiseXorAssignmentExpr =
    CBitwiseXorAssignmentExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
