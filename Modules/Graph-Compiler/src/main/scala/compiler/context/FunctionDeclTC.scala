package compiler.context

import clangast.decl.CFunctionDecl

trait FunctionDeclTC extends TranslationContext {
  val nameToFunctionDecl: MappingLabel[String, CFunctionDecl] = MappingLabel(valueDecls.append)
}
