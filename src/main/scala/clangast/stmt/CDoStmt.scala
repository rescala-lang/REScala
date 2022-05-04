package clangast.stmt

import clangast.expr.CExpr

case class CDoStmt(cond: CExpr, body: List[CStmt]) extends CStmt {
  override def textgen: String =
    s"""
       |do {
       |${body.map(_.textgen).mkString("\n").indent(2).stripTrailing()}
       |} while(${cond.textgen})
    """.strip().stripMargin
}
