package clangast.expr

import clangast.types.CQualType

case class CCastExpr(subExpr: CExpr, targetType: CQualType) extends CExpr {
  override def textgen: String = s"(${targetType.textgen}) ${subExpr.textgen}"
}
