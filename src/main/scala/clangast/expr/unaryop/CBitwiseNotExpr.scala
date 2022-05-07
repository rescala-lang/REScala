package clangast.expr.unaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CBitwiseNotExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "~"

  override def toExpr(using Quotes): Expr[CBitwiseNotExpr] = {
    val operandExpr = operand.toExpr

    '{ CBitwiseNotExpr($operandExpr) }
  }
}
