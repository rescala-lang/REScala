package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}
import compiler.context.IncludeTC

trait CLibraryStub {
  val include: CInclude

  protected def includeStub[T](stub: T)(using ctx: IncludeTC): T = {
    ctx.includes.add(this.include)
    stub
  }
}
