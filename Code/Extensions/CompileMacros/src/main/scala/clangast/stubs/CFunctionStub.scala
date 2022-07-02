package clangast.stubs

import clangast.given
import clangast.decl.CFunctionDecl
import clangast.types.CVoidType

object CFunctionStub {
  def apply(name: String): CFunctionDecl = CFunctionDecl(name, Nil, CVoidType)
}
