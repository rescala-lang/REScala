package clangast.types

import clangast.toExpr
import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CArrayType(elementType: CQualType, sizeExpr: Option[CExpr] = None) extends CType {
  override def textgen: String = sizeExpr match
    case None => elementType.textgen + "[]"
    case Some(expr) => elementType.textgen + "[" + expr.textgen + "]"

  override def typedVar(name: String): String = sizeExpr match
    case None => s"${elementType.textgen} $name[]"
    case Some(expr) => s"${elementType.textgen} $name[${expr.textgen}]"

  override def toExpr(using Quotes): Expr[CArrayType] = {
    val elementTypeExpr = elementType.toExpr
    val sizeExprExpr = sizeExpr.map(_.toExpr).toExpr

    '{ CArrayType($elementTypeExpr, $sizeExprExpr) }
  }
}
