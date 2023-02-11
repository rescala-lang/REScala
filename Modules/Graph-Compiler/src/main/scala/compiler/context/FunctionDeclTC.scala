package compiler.context

import clangast.decl.CFunctionDecl

import scala.collection.mutable

trait FunctionDeclTC extends TranslationContext {
  val nameToFunctionDecl: MappingLabel[String, CFunctionDecl] = MappingLabel(valueDecls.append)
}
