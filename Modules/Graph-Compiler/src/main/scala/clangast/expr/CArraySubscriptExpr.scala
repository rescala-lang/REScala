package clangast.expr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CArraySubscriptExpr(base: CExpr, idx: CExpr) extends CExpr {
  override def textgen: String = s"${base.textgen}[${idx.textgen}]"

  override def toExpr(using Quotes): Expr[CArraySubscriptExpr] = {
    val baseExpr = base.toExpr
    val idxExpr  = idx.toExpr

    '{ CArraySubscriptExpr($baseExpr, $idxExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CArraySubscriptExpr =
    CArraySubscriptExpr(mapper.mapCExpr(base), mapper.mapCExpr(idx))
}
