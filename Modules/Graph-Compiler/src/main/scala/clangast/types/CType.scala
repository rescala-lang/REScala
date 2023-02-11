package clangast.types

import clangast.CASTNode
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

trait CType extends CASTNode {
  def typedVar(name: String): String = s"$textgen $name"

  override def toExpr(using Quotes): Expr[CType]

  override def mapChildren(mapper: CASTMapper): CType = this
}
