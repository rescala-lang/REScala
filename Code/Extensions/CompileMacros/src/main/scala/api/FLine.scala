package api

import clangast.WithContext
import clangast.decl.CFunctionDecl

case class FLine[A, V](input: Event[A], f: WithContext[CFunctionDecl])
