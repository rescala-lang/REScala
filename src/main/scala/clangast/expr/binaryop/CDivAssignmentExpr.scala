package clangast.expr.binaryop

import clangast.expr.CExpr

case class CDivAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "/="
}
