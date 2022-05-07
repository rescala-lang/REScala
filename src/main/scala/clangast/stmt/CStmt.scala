package clangast.stmt

import clangast.CASTNode

import scala.quoted.{Expr, Quotes}

trait CStmt extends CASTNode {
  override def toExpr(using Quotes): Expr[CStmt]
}
