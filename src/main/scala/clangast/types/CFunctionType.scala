package clangast.types

case class CFunctionType(paramTypes: List[CQualType], returnType: CQualType) extends CType
