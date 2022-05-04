package clangast.expr.binaryop

import clangast.expr.CExpr

case class CModAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "%="
}
