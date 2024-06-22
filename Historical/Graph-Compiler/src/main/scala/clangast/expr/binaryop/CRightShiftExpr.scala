package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CRightShiftExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = ">>"

  override def toExpr(using Quotes): Expr[CRightShiftExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CRightShiftExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CRightShiftExpr =
    CRightShiftExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
