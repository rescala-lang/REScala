package clangast.stmt

import clangast.CASTNode
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

trait CStmt extends CASTNode {
  override def toExpr(using Quotes): Expr[CStmt]

  override def mapChildren(mapper: CASTMapper): CStmt = this
}
