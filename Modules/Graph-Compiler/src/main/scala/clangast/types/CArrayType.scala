package clangast.types

import clangast.expr.CExpr
import clangast.toExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CArrayType(elementType: CQualType, sizeExpr: Option[CExpr] = None) extends CType {
  override def textgen: String = sizeExpr match
    case None       => elementType.textgen + "[]"
    case Some(expr) => elementType.textgen + "[" + expr.textgen + "]"

  override def typedVar(name: String): String = sizeExpr match
    case None       => s"${elementType.textgen} $name[]"
    case Some(expr) => s"${elementType.textgen} $name[${expr.textgen}]"

  override def toExpr(using Quotes): Expr[CArrayType] = {
    val elementTypeExpr = elementType.toExpr
    val sizeExprExpr    = sizeExpr.map(_.toExpr).toExpr

    '{ CArrayType($elementTypeExpr, $sizeExprExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CArrayType =
    CArrayType(mapper.mapCQualType(elementType), sizeExpr.map(mapper.mapCExpr))
}
