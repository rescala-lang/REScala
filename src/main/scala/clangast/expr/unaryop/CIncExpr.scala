package clangast.expr.unaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CIncExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "++"

  override def toExpr(using Quotes): Expr[CIncExpr] = {
    val operandExpr = operand.toExpr

    '{ CIncExpr($operandExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CIncExpr =
    CIncExpr(mapper.mapCExpr(operand))
}
