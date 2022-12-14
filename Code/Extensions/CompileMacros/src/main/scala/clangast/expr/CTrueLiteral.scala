package clangast.expr

import scala.quoted.*

case object CTrueLiteral extends CExpr {
  override def textgen: String = "true"

  override def toExpr(using Quotes): Expr[CTrueLiteral.type] = '{ CTrueLiteral }
}
