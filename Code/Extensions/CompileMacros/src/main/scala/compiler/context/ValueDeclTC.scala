package compiler.context

import clangast.decl.CValueDecl

import scala.collection.mutable

trait ValueDeclTC extends TranslationContext {
  val nameToDecl: MappingLabel[String, CValueDecl] = MappingLabel()
}
