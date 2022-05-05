package clangast.expr.unaryop

import clangast.expr.CExpr

case class CUnaryMinusExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "-"
}
