package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CPlusAssignmentExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "+="

  override def toExpr(using Quotes): Expr[CPlusAssignmentExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CPlusAssignmentExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CPlusAssignmentExpr =
    CPlusAssignmentExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
