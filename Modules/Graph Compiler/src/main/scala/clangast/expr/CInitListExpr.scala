package clangast.expr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CInitListExpr(inits: List[CExpr]) extends CExpr {
  override def textgen: String = "{" + inits.map(_.textgen).mkString(", ") + "}"

  override def toExpr(using Quotes): Expr[CInitListExpr] = {
    val initsExpr = Expr.ofList(inits.map(_.toExpr))

    '{ CInitListExpr($initsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CInitListExpr =
    CInitListExpr(inits.map(mapper.mapCExpr))
}
