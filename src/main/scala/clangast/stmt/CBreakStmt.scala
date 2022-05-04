package clangast.stmt

case object CBreakStmt extends CStmt {
  override def textgen: String = "break;"
}
