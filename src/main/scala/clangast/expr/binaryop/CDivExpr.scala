package clangast.expr.binaryop

import clangast.expr.CExpr

case class CDivExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "/"
}
