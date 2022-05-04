package clangast.expr.binaryop

import clangast.expr.CExpr

case class CGreaterEqualsExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = ">="
}
