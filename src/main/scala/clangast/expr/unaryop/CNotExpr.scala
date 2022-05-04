package clangast.expr.unaryop

import clangast.expr.CExpr

case class CNotExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "!"
}
