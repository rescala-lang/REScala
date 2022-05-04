package clangast.expr

case class CArraySubscriptExpr(base: CExpr, idx: CExpr) extends CExpr {
  override def textgen: String = s"${base.textgen}[${idx.textgen}]"
}
