package clangast.expr

case class CDoubleLiteral(d: Double) extends CExpr {
  override def textgen: String = d.toString
}
