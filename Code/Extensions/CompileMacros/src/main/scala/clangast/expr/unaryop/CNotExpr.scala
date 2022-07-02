package clangast.expr.unaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CNotExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "!"

  override def toExpr(using Quotes): Expr[CNotExpr] = {
    val operandExpr = operand.toExpr

    '{ CNotExpr($operandExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CNotExpr =
    CNotExpr(mapper.mapCExpr(operand))
}
