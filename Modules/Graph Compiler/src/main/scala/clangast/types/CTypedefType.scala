package clangast.types

import clangast.decl.CTypedefDecl

import scala.quoted.{Expr, Quotes}

case class CTypedefType(declName: String) extends CType {
  override def textgen: String = declName

  override def toExpr(using Quotes): Expr[CTypedefType] = {
    val declNameExpr = Expr(declName)

    '{ CTypedefType($declNameExpr) }
  }
}
