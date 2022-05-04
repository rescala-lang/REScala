package clangast.expr

case class CStringLiteral(s: String) extends CExpr {
  override def textgen: String = '"' + s + '"'
}
