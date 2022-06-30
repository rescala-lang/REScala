package clangast.stubs

import clangast.decl.CInclude
import clangast.types.{CBoolType, CType}
import compiler.context.TranslationContext

object StdBoolH extends CLibraryStub {
  override val include: CInclude = CInclude("stdbool.h")

  def bool(using ctx: TranslationContext): CType = includeStub(CBoolType)
}
