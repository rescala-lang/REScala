package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class COrExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "||"

  override def toExpr(using Quotes): Expr[COrExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ COrExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): COrExpr =
    COrExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
