package clangast.expr.binaryop

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CBitwiseAndExpr(lhs: CExpr, rhs: CExpr) extends CBinaryOperator {
  val opcode = "&"

  override def toExpr(using Quotes): Expr[CBitwiseAndExpr] = {
    val lhsExpr = lhs.toExpr
    val rhsExpr = rhs.toExpr

    '{ CBitwiseAndExpr($lhsExpr, $rhsExpr) }
  }
}
