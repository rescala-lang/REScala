package clangast.stubs

import clangast.given
import clangast.decl.{CFunctionDecl, CInclude, CTypedefDecl}
import clangast.types.CVoidType
import compiler.context.IncludeTC

object StdArgH extends CLibraryStub {
  override val include: CInclude = CInclude("stdarg.h")
  
  def va_list(using IncludeTC): CTypedefDecl = includeStub(CTypedefDecl("va_list", CVoidType))
  
  def va_start(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("va_start"))
  def va_arg(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("va_arg"))
  def va_end(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("va_end"))
}
