package clangast.expr

case class CParenExpr(subExpr: CExpr) extends CExpr {
  override def textgen: String = s"(${subExpr.textgen})"
}
