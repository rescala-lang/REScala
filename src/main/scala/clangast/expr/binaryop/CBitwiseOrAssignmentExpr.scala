package clangast.expr.binaryop

import clangast.expr.CExpr

case class CBitwiseOrAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "|="
}
