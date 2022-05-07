package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

trait CBinaryOperator extends CExpr {
  val opcode: String
  val lhs: CExpr
  val rhs: CExpr

  override def textgen: String = s"${lhs.textgen} $opcode ${rhs.textgen}"

  override def toExpr(using Quotes): Expr[CBinaryOperator]
}
