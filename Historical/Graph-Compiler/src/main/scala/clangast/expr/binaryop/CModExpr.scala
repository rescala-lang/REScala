package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CModExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "%"

  override def toExpr(using Quotes): Expr[CModExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CModExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CModExpr =
    CModExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
