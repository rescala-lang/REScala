package compiler.context

import clangast.decl.CValueDecl

trait ValueDeclTC extends TranslationContext {
  val nameToDecl: MappingLabel[String, CValueDecl] = MappingLabel()
}
