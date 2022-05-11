package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CProdExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "*"

  override def toExpr(using Quotes): Expr[CProdExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CProdExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CProdExpr =
    CProdExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
