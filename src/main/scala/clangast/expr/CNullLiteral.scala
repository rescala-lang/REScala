package clangast.expr

case object CNullLiteral extends CExpr {
  override def textgen: String = "NULL"
}
