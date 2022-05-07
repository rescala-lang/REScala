package clangast.expr

import clangast.decl.CValueDecl

import scala.quoted.{Expr, Quotes}

case class CDeclRefExpr(decl: CValueDecl) extends CExpr {
  override def textgen: String = decl.name

  override def toExpr(using Quotes): Expr[CDeclRefExpr] = {
    val declExpr = decl.toExpr

    '{ CDeclRefExpr($declExpr) }
  }
}
