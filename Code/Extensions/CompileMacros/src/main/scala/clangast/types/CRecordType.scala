package clangast.types

import clangast.decl.CRecordDecl

import scala.quoted.{Expr, Quotes}

case class CRecordType(declName: String) extends CType {
  override def textgen: String = declName

  override def toExpr(using Quotes): Expr[CRecordType] = {
    val declNameExpr = Expr(declName)

    '{ CRecordType($declNameExpr) }
  }
}
