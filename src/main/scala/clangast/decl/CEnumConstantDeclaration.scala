package clangast.decl

import clangast.expr.CExpr
import clangast.types.CQualType

case class CEnumConstantDeclaration(name: String, initExpr: Option[CExpr]) extends CValueDecl {
  override def getType: CQualType = ???
  
  override def textgen: String = initExpr match {
    case None => name
    case Some(expr) => name + " = " + expr.textgen
  }
}
