package clangast.expr.unaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CUnaryPlusExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "+"

  override def toExpr(using Quotes): Expr[CUnaryPlusExpr] = {
    val operandExpr = operand.toExpr

    '{ CUnaryPlusExpr($operandExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CUnaryPlusExpr =
    CUnaryPlusExpr(mapper.mapCExpr(operand))
}
