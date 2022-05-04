package clangast.stmt

import clangast.expr.CExpr

case class CCaseStmt(cond: CExpr, body: List[CStmt]) extends CSwitchCase {
  override def textgen: String =
    s"""
       |case ${cond.textgen}:
       |${body.map(_.textgen).mkString("\n").indent(2).stripTrailing()}
    """.strip().stripMargin
}
