package clangast.types

case object CIntegerType extends CBuiltinType {
  override def textgen: String = "int"
}
