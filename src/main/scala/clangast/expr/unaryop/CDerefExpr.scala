package clangast.expr.unaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CDerefExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "*"

  override def toExpr(using Quotes): Expr[CDerefExpr] = {
    val operandExpr = operand.toExpr

    '{ CDerefExpr($operandExpr) }
  }
}
