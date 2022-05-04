package clangast.decl

import clangast.types.{CQualType, CType, CTypedefType}

case class CTypedefDecl(name: String, underlyingType: CQualType) extends CTypeDecl {
  override def getTypeForDecl: CType = CTypedefType(this)

  override def textgen: String = s"typedef ${underlyingType.textgen} $name;"
}
