package clangast

import clangast.decl.CInclude

object StdIncludes {
  val math: CInclude = CInclude("math.h")
  val stdbool: CInclude = CInclude("stdbool.h")
  val stdio: CInclude = CInclude("stdio.h")
  val stdlib: CInclude = CInclude("stdlib.h")
  val string: CInclude = CInclude("string.h")
}
