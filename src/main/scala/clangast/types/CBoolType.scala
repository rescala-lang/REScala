package clangast.types

case object CBoolType extends CBuiltinType {
  override def textgen: String = "bool"
}
