package clangast.expr.binaryop

import clangast.expr.CExpr

case class CBitwiseAndExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "&"
}
