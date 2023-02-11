package clangast.expr.unaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CUnaryMinusExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "-"

  override def toExpr(using Quotes): Expr[CUnaryMinusExpr] = {
    val operandExpr = operand.toExpr

    '{ CUnaryMinusExpr($operandExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CUnaryMinusExpr =
    CUnaryMinusExpr(mapper.mapCExpr(operand))
}
