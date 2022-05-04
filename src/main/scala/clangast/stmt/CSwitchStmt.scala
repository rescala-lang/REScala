package clangast.stmt

import clangast.expr.CExpr

case class CSwitchStmt(cond: CExpr, cases: List[CSwitchCase]) extends CStmt {
  override def textgen: String =
    s"""
       |switch (${cond.textgen}) {
       |${cases.map(_.textgen).mkString("\n\n").indent(2).stripTrailing()}
       |}
    """.strip().stripMargin
}
