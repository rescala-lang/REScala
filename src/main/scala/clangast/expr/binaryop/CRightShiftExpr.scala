package clangast.expr.binaryop

import clangast.expr.CExpr

case class CRightShiftExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = ">>"
}
