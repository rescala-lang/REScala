package clangast.decl

import clangast.types.{CEnumType, CQualType, CType}

case class CEnumDecl(name: String, integerType: CQualType, enumConstants: List[CEnumConstantDeclaration]) extends CTypeDecl with CDeclContext {
  override def getTypeForDecl: CType = CEnumType(this)
  
  override def decls: List[CDecl] = enumConstants

  override def textgen: String =
    s"enum $name {" + enumConstants.map(_.textgen).mkString(", ") + "}"
}