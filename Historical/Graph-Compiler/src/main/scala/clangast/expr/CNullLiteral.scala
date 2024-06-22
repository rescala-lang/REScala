package clangast.expr

import scala.quoted.*

case object CNullLiteral extends CExpr {
  override def textgen: String = "NULL"

  override def toExpr(using Quotes): Expr[CNullLiteral.type] = '{ CNullLiteral }
}
