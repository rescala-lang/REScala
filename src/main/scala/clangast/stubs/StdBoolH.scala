package clangast.stubs

import clangast.decl.CInclude

object StdBoolH extends CLibraryStub {
  override val include: CInclude = CInclude("stdbool.h")
}
