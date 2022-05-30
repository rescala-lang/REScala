package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}

object StdIOH {
  val include: CInclude = CInclude("stdio.h")

  val printf: CFunctionDecl = CFunctionStub("printf")
  val sprintf: CFunctionDecl = CFunctionStub("sprintf")
  val scanf: CFunctionDecl = CFunctionStub("scanf")
  val sscanf: CFunctionDecl = CFunctionStub("sscanf")
}
