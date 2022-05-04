package clangast.expr

import clangast.decl.CValueDecl

case class CMemberExpr(base: CExpr, memberDecl: CValueDecl, isArrow: Boolean = false) extends CExpr {
  override def textgen: String =
    if isArrow then base.textgen + "->" + memberDecl.name
    else base.textgen + "." + memberDecl.name
}
