package clangast.expr

case class CConditionalOperator(cond: CExpr, trueExpr: CExpr, falseExpr: CExpr) extends CExpr {
  override def textgen: String = s"${cond.textgen} ? ${trueExpr.textgen} : ${falseExpr.textgen}"
}
