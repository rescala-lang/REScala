package clangast.expr.binaryop

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CBitwiseXorExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "^"

  override def toExpr(using Quotes): Expr[CBitwiseXorExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CBitwiseXorExpr($lhsExpr, $rhsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CBitwiseXorExpr =
    CBitwiseXorExpr(mapper.mapCExpr(lhs), mapper.mapCExpr(rhs))
}
