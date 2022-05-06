package clangast.types

case object CCharType extends CBuiltinType {
  override def textgen: String = "char"
}
