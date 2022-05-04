package clangast.types

import clangast.decl.CTypedefDecl

case class CTypedefType(decl: CTypedefDecl) extends CType {
  override def textgen: String = decl.name
}
