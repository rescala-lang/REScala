package clangast.expr

import scala.quoted.*

case object CFalseLiteral extends CExpr {
  override def textgen: String = "false"

  override def toExpr(using Quotes): Expr[CFalseLiteral.type] = '{ CFalseLiteral }
}
