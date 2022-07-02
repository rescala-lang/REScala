package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CNotEqualsExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "!="

  override def toExpr(using Quotes): Expr[CNotEqualsExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CNotEqualsExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CNotEqualsExpr =
    CNotEqualsExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
