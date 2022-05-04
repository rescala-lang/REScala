package clangast.stmt

import clangast.decl.CDecl

case class CDeclStmt(decl: CDecl) extends CStmt {
  override def textgen: String = decl.textgen
}