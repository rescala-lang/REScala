package clangast.expr.unaryop

import clangast.expr.CExpr

case class CUnaryPlusExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "+"
}
