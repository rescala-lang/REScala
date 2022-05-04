package clangast.expr.unaryop

import clangast.expr.CExpr

case class CIncExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "++"
}
