package clangast.expr

import clangast.types.CQualType

import scala.quoted.{Expr, Quotes}

case class CCastExpr(subExpr: CExpr, targetType: CQualType) extends CExpr {
  override def textgen: String = s"(${targetType.textgen}) ${subExpr.textgen}"

  override def toExpr(using Quotes): Expr[CCastExpr] = {
    val subExprExpr = subExpr.toExpr
    val targetTypeExpr = targetType.toExpr

    '{ CCastExpr($subExprExpr, $targetTypeExpr) }
  }
}
