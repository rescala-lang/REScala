package clangast.expr

case class CCharacterLiteral(char: Char) extends CExpr {
  override def textgen: String = s"'$char'"
}
