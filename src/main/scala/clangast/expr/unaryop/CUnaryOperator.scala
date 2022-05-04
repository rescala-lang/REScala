package clangast.expr.unaryop

import clangast.expr.CExpr

trait CUnaryOperator extends CExpr {
  val opcode: String
  val operand: CExpr

  override def textgen: String = opcode + operand.textgen
}
