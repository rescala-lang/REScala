package clangast.decl

case class CTranslationUnitDecl(children: List[CDecl]) extends CDecl with CDeclContext {
  override def decls: List[CDecl] = children

  override def textgen: String = children.map(_.textgen).mkString("\n\n")
}
