package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}
import compiler.context.TranslationContext

trait CLibraryStub {
  val include: CInclude

  protected def includeStub[T](stub: T)(using ctx: TranslationContext): T = {
    ctx.addInclude(this.include)
    stub
  }
}
