package clangast.expr.unaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CUnaryMinusExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "-"

  override def toExpr(using Quotes): Expr[CUnaryMinusExpr] = {
    val operandExpr = operand.toExpr

    '{ CUnaryMinusExpr($operandExpr) }
  }
}
