package clangast.types

case object CShortType extends CBuiltinType {
  override def textgen: String = "short"
}
