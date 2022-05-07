package clangast.decl

import clangast.CASTNode

import scala.quoted.*

trait CDecl extends CASTNode {
  override def toExpr(using Quotes): Expr[CDecl]
}
