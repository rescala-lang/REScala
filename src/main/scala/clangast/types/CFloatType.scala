package clangast.types

case object CFloatType extends CBuiltinType {
  override def textgen: String = "float"
}
