package clangast.decl

import clangast.CASTNode
import clangast.traversal.CASTMapper

import scala.quoted.*

trait CDecl extends CASTNode {
  override def toExpr(using Quotes): Expr[CDecl]

  override def mapChildren(mapper: CASTMapper): CDecl = this
}
