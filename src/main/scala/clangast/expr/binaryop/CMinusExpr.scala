package clangast.expr.binaryop

import clangast.expr.CExpr

case class CMinusExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "-"
}
