package clangast.expr.unaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CDecExpr(operand: CExpr) extends CUnaryOperator {
  val opcode: String = "--"

  override def toExpr(using Quotes): Expr[CDecExpr] = {
    val operandExpr = operand.toExpr

    '{ CDecExpr($operandExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CDecExpr =
    CDecExpr(mapper.mapCExpr(operand))
}
