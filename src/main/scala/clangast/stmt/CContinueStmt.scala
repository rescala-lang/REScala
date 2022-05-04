package clangast.stmt

case object CContinueStmt extends CStmt {
  override def textgen: String = "continue;"
}
