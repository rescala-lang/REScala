package clangast.types

import clangast.CASTNode

case class CQualType(unqualType: CType, isConst: Boolean = false) extends CASTNode {
  override def textgen: String =
    (if isConst then "const " else "") + unqualType.textgen
    
  def typedVar(name: String): String =
    (if isConst then "const " else "") + unqualType.typedVar(name)
}
