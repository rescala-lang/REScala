package clangast.expr
import scala.quoted.{Expr, Quotes}

case class CArraySubscriptExpr(base: CExpr, idx: CExpr) extends CExpr {
  override def textgen: String = s"${base.textgen}[${idx.textgen}]"

  override def toExpr(using Quotes): Expr[CArraySubscriptExpr] = {
    val baseExpr = base.toExpr
    val idxExpr = idx.toExpr

    '{ CArraySubscriptExpr($baseExpr, $idxExpr) }
  }
}
