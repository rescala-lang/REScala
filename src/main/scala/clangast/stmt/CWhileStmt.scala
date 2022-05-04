package clangast.stmt

import clangast.expr.CExpr

case class CWhileStmt(cond: CExpr, body: List[CStmt]) extends CStmt {
  override def textgen: String =
    s"""
       |while(${cond.textgen}) {
       |${body.map(_.textgen).mkString("\n").indent(2).stripTrailing()}
       |}
    """.strip().stripMargin
}
