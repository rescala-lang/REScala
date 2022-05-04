package clangast.expr.unaryop

import clangast.expr.CExpr

case class CBitwiseNotExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "~"
}
