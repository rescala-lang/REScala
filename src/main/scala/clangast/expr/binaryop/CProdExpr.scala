package clangast.expr.binaryop

import clangast.expr.CExpr

case class CProdExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "*"
}
