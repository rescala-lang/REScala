package clangast.expr.binaryop

import clangast.expr.CExpr

case class CBitwiseXorExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "^"
}
