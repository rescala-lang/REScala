package clangast.expr

import clangast.CASTNode

import scala.quoted.{Expr, Quotes}

trait CExpr extends CASTNode {
  override def toExpr(using Quotes): Expr[CExpr]
}
