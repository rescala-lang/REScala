package clangast.expr

case class CFloatLiteral(f: Float) extends CExpr {
  override def textgen: String = f + "f"
}
