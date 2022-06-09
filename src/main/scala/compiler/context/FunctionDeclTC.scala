package compiler.context

import clangast.decl.CFunctionDecl

import scala.collection.mutable

trait FunctionDeclTC extends TranslationContext {
  val nameToFunctionDecl: mutable.Map[String, CFunctionDecl] = mutable.Map()
}
