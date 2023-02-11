package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CEqualsExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "=="

  override def toExpr(using Quotes): Expr[CEqualsExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CEqualsExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CEqualsExpr =
    CEqualsExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
