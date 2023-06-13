package clangast.expr


import scala.quoted.{Expr, Quotes}

case class CDeclRefExpr(declName: String) extends CExpr {
  override def textgen: String = declName

  override def toExpr(using Quotes): Expr[CDeclRefExpr] = {
    val declNameExpr = Expr(declName)

    '{ CDeclRefExpr($declNameExpr) }
  }
}
