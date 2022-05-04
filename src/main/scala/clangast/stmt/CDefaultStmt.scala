package clangast.stmt

case class CDefaultStmt(body: List[CStmt]) extends CSwitchCase {
  override def textgen: String =
    s"""
       |default:
       |${body.map(_.textgen).mkString("\n").indent(2).stripTrailing()}
    """.strip().stripMargin
}
