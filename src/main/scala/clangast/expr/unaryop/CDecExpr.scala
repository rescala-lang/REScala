package clangast.expr.unaryop

import clangast.expr.CExpr

case class CDecExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "--"
}
