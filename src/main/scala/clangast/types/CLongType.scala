package clangast.types

case object CLongType extends CBuiltinType {
  override def textgen: String = "long"
}
