package clangast.stmt

import clangast.expr.CExpr

case class CIfStmt(cond: CExpr, thenBranch: CStmt, elseBranch: Option[CStmt] = None) extends CStmt {
  override def textgen: String = {
    val thenPart = s"if (${cond.textgen}) ${thenBranch.textgen}"

    elseBranch match
      case None => thenPart
      case Some(stmt) => thenPart + " else " + stmt.textgen
  }
}
