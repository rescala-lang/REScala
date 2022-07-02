package clangast.types

import clangast.decl.CEnumDecl

import scala.quoted.{Expr, Quotes}

case class CEnumType(declName: String) extends CType {
  override def textgen: String = s"enum $declName"

  override def toExpr(using Quotes): Expr[CEnumType] = {
    val declNameExpr = Expr(declName)

    '{ CEnumType($declNameExpr) }
  }
}
