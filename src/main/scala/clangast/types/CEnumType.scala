package clangast.types

import clangast.decl.CEnumDecl

case class CEnumType(decl: CEnumDecl) extends CType {
  override def textgen: String = s"enum ${decl.name}"
}
