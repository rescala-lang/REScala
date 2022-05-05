package clangast.expr

import clangast.stmt.CCompoundStmt

case class CStmtExpr(subStmt: CCompoundStmt) extends CExpr {
  override def textgen: String = "(" + subStmt.textgen + ")"
}
