package clangast.expr.binaryop

import clangast.expr.CExpr

case class CBitwiseAndAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "&="
}
