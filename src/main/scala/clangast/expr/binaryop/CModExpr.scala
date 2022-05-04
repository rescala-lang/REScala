package clangast.expr.binaryop

import clangast.expr.CExpr

case class CModExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "%"
}
