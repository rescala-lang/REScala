package clangast.expr.binaryop

import clangast.expr.CExpr

trait CBinaryOperator extends CExpr {
  val opcode: String
  val lhs: CExpr
  val rhs: CExpr

  override def textgen: String = s"${lhs.textgen} $opcode ${rhs.textgen}"
}
