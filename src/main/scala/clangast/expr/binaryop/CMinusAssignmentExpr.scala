package clangast.expr.binaryop

import clangast.expr.CExpr

case class CMinusAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "-="
}
