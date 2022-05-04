package clangast.expr.binaryop

import clangast.expr.CExpr

case class CBitwiseOrExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "|"
}
