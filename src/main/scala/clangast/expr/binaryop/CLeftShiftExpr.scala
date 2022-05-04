package clangast.expr.binaryop

import clangast.expr.CExpr

case class CLeftShiftExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "<<"
}
