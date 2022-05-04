package clangast.expr

case class CIntegerLiteral(v: Int) extends CExpr {
  override def textgen: String = v.toString
}
