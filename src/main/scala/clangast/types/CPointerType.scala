package clangast.types

case class CPointerType(pointeeType: CQualType) extends CType {
  override def textgen: String = pointeeType.textgen + "*"
}
