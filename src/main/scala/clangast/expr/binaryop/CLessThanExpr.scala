package clangast.expr.binaryop

import clangast.expr.CExpr

case class CLessThanExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "<"
}
