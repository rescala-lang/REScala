package clangast.expr.binaryop

import clangast.expr.CExpr

case class CAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "="
}
