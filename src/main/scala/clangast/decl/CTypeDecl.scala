package clangast.decl

import clangast.types.CType

trait CTypeDecl extends CNamedDecl {
  def getTypeForDecl: CType
}
