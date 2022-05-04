package clangast.expr.unaryop

import clangast.expr.CExpr

case class CDerefExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "*"
}
