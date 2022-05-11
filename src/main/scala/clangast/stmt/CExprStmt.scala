package clangast.stmt

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CExprStmt(expr: CExpr) extends CStmt {
  override def textgen: String = expr.textgen + ";"

  override def toExpr(using Quotes): Expr[CExprStmt] = {
    val exprExpr = expr.toExpr

    '{ CExprStmt($exprExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CExprStmt =
    CExprStmt(mapper.mapCExpr(expr))
}
