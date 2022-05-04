package clangast.expr

import clangast.decl.CValueDecl

case class CDeclRefExpr(decl: CValueDecl) extends CExpr {
  override def textgen: String = decl.name
}
