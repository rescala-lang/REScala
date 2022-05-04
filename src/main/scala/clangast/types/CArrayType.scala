package clangast.types

import clangast.expr.CExpr

case class CArrayType(elementType: CQualType, sizeExpr: Option[CExpr] = None) extends CType {
  override def textgen: String = sizeExpr match
    case None => elementType.textgen + "[]"
    case Some(expr) => elementType.textgen + "[" + expr.textgen + "]"

  override def typedVar(name: String): String = sizeExpr match
    case None => s"${elementType.textgen} $name[]"
    case Some(expr) => s"${elementType.textgen} $name[${expr.textgen}]"
}
