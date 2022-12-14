package clangast.expr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CDesignatedInitExpr(inits: List[(String, CExpr)]) extends CExpr {
  override def textgen: String =
    "{" +
    inits.map { case (fieldName, expr) => s".$fieldName = ${expr.textgen}" }.mkString(", ") +
    "}"

  override def toExpr(using Quotes): Expr[CDesignatedInitExpr] = {
    val initsExpr = Expr.ofList(inits.map {
      case (str, expr) =>
        val strExpr  = Expr(str)
        val exprExpr = expr.toExpr

        Expr.ofTuple((strExpr, exprExpr))
    })

    '{ CDesignatedInitExpr($initsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CDesignatedInitExpr =
    CDesignatedInitExpr(inits.map {
      case (designator, expr) => (designator, mapper.mapCExpr(expr))
    })
}
