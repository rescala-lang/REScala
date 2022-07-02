package clangast.expr

import clangast.stmt.CCompoundStmt
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CStmtExpr(subStmt: CCompoundStmt) extends CExpr {
  override def textgen: String = "(" + subStmt.textgen + ")"

  override def toExpr(using Quotes): Expr[CStmtExpr] = {
    val subStmtExpr = subStmt.toExpr

    '{ CStmtExpr($subStmtExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CStmtExpr =
    CStmtExpr(mapper.mapCCompoundStmt(subStmt))
}
