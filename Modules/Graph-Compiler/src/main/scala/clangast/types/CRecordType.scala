package clangast.types

import scala.quoted.{Expr, Quotes}

case class CRecordType(declName: String) extends CType {
  override def textgen: String = declName

  override def toExpr(using Quotes): Expr[CRecordType] = {
    val declNameExpr = Expr(declName)

    '{ CRecordType($declNameExpr) }
  }
}
