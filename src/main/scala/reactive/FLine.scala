package reactive

import clangast.decl.CFunctionDecl

case class FLine[A, V](input: Event[A], f: CFunctionDecl)
