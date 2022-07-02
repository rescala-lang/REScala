package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CGreaterEqualsExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = ">="

  override def toExpr(using Quotes): Expr[CGreaterEqualsExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CGreaterEqualsExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CGreaterEqualsExpr =
    CGreaterEqualsExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
