package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CBitwiseAndExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "&"

  override def toExpr(using Quotes): Expr[CBitwiseAndExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CBitwiseAndExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CBitwiseAndExpr =
    CBitwiseAndExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
