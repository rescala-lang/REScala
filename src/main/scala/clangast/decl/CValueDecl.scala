package clangast.decl

import clangast.types.CQualType

trait CValueDecl extends CNamedDecl {
  def getType: CQualType
}
