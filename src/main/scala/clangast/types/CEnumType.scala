package clangast.types

import clangast.decl.CEnumDecl

import scala.quoted.{Expr, Quotes}

case class CEnumType(decl: CEnumDecl) extends CType {
  override def textgen: String = s"enum ${decl.name}"

  override def toExpr(using Quotes): Expr[CEnumType] = {
    val declExpr = decl.toExpr

    '{ CEnumType($declExpr) }
  }
}
