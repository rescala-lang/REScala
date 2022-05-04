package clangast.expr.binaryop

import clangast.expr.CExpr

case class CBitwiseXorAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "^="
}
