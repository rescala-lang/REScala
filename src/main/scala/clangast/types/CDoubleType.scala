package clangast.types

case object CDoubleType extends CBuiltinType {
  override def textgen: String = "double"
}
