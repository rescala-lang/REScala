package clangast.decl

import clangast.expr.CExpr
import clangast.types.CQualType

case class CVarDecl(name: String, declaredType: CQualType, init: Option[CExpr] = None) extends CValueDecl {
  override def getType: CQualType = declaredType

  override def textgen: String = {
    val decl = declaredType.typedVar(name)
    
    init match {
      case None => decl + ";"
      case Some(expr) => decl + s" = ${expr.textgen};"
    }
  }
}
