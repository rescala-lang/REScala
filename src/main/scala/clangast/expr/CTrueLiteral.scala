package clangast.expr

case object CTrueLiteral extends CExpr {
  override def textgen: String = "true"
}
