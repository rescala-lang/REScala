package clangast.expr.unaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CAddressExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "&"

  override def toExpr(using Quotes): Expr[CAddressExpr] = {
    val operandExpr = operand.toExpr

    '{ CAddressExpr($operandExpr) }
  }
}
