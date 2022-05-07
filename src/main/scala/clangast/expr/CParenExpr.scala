package clangast.expr
import scala.quoted.{Expr, Quotes}

case class CParenExpr(subExpr: CExpr) extends CExpr {
  override def textgen: String = s"(${subExpr.textgen})"

  override def toExpr(using Quotes): Expr[CParenExpr] = {
    val subExprExpr = subExpr.toExpr

    '{ CParenExpr($subExprExpr) }
  }
}
