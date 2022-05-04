package clangast.expr.binaryop

import clangast.expr.CExpr

case class CGreaterThanExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = ">"
}
