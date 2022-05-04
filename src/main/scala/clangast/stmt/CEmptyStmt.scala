package clangast.stmt

case object CEmptyStmt extends CStmt {
  override def textgen: String = ""
}
