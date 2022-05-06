package clangast.stmt

import clangast.expr.CExpr

case class CWhileStmt(cond: CExpr, body: CCompoundStmt) extends CStmt {
  override def textgen: String = s"while (${cond.textgen}) ${body.textgen}"
}
