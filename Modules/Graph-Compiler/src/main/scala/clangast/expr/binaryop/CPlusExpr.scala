package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.annotation.targetName
import scala.quoted.{Expr, Quotes}

case class CPlusExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "+"

  override def toExpr(using Quotes): Expr[CPlusExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CPlusExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CPlusExpr =
    CPlusExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
