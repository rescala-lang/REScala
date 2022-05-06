package clangast.expr

case class CLongLiteral(v: Long) extends CExpr {
  override def textgen: String = v + "L"
}
