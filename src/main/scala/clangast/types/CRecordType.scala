package clangast.types

import clangast.decl.CRecordDecl

case class CRecordType(decl: CRecordDecl) extends CType {
  override def textgen: String = decl.name
}
