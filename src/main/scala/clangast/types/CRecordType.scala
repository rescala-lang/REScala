package clangast.types

import clangast.decl.CRecordDecl

import scala.quoted.{Expr, Quotes}

case class CRecordType(decl: CRecordDecl) extends CType {
  override def textgen: String = decl.name

  override def toExpr(using Quotes): Expr[CRecordType] = {
    val declExpr = decl.toExpr

    '{ CRecordType($declExpr) }
  }
}
