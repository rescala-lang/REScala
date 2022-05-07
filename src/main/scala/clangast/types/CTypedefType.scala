package clangast.types

import clangast.decl.CTypedefDecl

import scala.quoted.{Expr, Quotes}

case class CTypedefType(decl: CTypedefDecl) extends CType {
  override def textgen: String = decl.name

  override def toExpr(using Quotes): Expr[CTypedefType] = {
    val declExpr = decl.toExpr

    '{ CTypedefType($declExpr) }
  }
}
