package clangast.expr.unaryop

import clangast.expr.CExpr

case class CAddressExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "&"
}
