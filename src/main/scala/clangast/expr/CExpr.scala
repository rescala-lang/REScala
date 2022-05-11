package clangast.expr

import clangast.CASTNode
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

trait CExpr extends CASTNode {
  override def toExpr(using Quotes): Expr[CExpr]

  override def mapChildren(mapper: CASTMapper): CExpr = this
}
