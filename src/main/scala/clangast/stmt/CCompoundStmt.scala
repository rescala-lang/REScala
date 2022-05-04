package clangast.stmt

case class CCompoundStmt(body: List[CStmt]) extends CStmt {
  override def textgen: String =
    s"""
       |{
       |${body.map(_.textgen).mkString("\n").indent(2).stripTrailing()}
       |}
    """.strip().stripMargin
}
