package clangast.expr

case class CInitListExpr(inits: List[CExpr]) extends CExpr {
  override def textgen: String = "{" + inits.map(_.textgen).mkString(", ") + "}"
}
