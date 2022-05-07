package clangast.expr
import scala.quoted.{Expr, Quotes}

case class CConditionalOperator(cond: CExpr, trueExpr: CExpr, falseExpr: CExpr) extends CExpr {
  override def textgen: String = s"${cond.textgen} ? ${trueExpr.textgen} : ${falseExpr.textgen}"

  override def toExpr(using Quotes): Expr[CConditionalOperator] = {
    val condExpr = cond.toExpr
    val trueExprExpr = trueExpr.toExpr
    val falseExprExpr = falseExpr.toExpr

    '{ CConditionalOperator($condExpr, $trueExprExpr, $falseExprExpr) }
  }
}
