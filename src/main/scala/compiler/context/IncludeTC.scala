package compiler.context

import clangast.decl.CInclude

import scala.collection.mutable

trait IncludeTC extends TranslationContext {
  val includes: mutable.Set[CInclude] = mutable.Set()
}
