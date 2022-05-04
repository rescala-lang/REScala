package clangast.stmt

case object CNullStmt extends CStmt {
  override def textgen: String = ";"
}
