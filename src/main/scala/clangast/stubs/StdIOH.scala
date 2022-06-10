package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}
import compiler.context.IncludeTC

object StdIOH extends CLibraryStub {
  override val include: CInclude = CInclude("stdio.h")

  def printf(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("printf"))
  def sprintf(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("sprintf"))
  def scanf(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("scanf"))
  def sscanf(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("sscanf"))
}
