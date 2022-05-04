package clangast.stmt

import clangast.expr.CExpr

case class CExprStmt(expr: CExpr) extends CStmt {
  override def textgen: String = expr.textgen + ";"
}
