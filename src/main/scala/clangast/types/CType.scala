package clangast.types

import clangast.CASTNode

import scala.quoted.{Expr, Quotes}

trait CType extends CASTNode {
  def typedVar(name: String): String = s"$textgen $name"

  override def toExpr(using Quotes): Expr[CType]
}
