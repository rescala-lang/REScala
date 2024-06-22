package clangast.expr.unaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

trait CUnaryOperator extends CExpr {
  val opcode: String
  val operand: CExpr

  override def textgen: String = opcode + operand.textgen

  override def toExpr(using Quotes): Expr[CUnaryOperator]

  override def mapChildren(mapper: CASTMapper): CUnaryOperator = this
}
