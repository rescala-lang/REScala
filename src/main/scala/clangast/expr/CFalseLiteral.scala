package clangast.expr

case object CFalseLiteral extends CExpr {
  override def textgen: String = "false"
}
