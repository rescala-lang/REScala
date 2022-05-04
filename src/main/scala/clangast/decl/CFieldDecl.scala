package clangast.decl
import clangast.types.CQualType

case class CFieldDecl(name: String, declaredType: CQualType) extends CValueDecl {
  override def getType: CQualType = declaredType

  override def textgen: String = s"${declaredType.textgen} $name;"
}
