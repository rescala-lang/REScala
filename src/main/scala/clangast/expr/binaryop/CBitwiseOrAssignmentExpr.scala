package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CBitwiseOrAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "|="

  override def toExpr(using Quotes): Expr[CBitwiseOrAssignmentExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CBitwiseOrAssignmentExpr($lhsExpr, $rhsExpr) }
  }
}
