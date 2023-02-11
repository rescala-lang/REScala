package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}
import compiler.context.TranslationContext

object StdIOH extends CLibraryStub {
  override val include: CInclude = CInclude("stdio.h")

  def printf(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("printf"))
  def sprintf(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("sprintf"))
  def snprintf(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("snprintf"))
  def scanf(using TranslationContext): CFunctionDecl    = includeStub(CFunctionStub("scanf"))
  def sscanf(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("sscanf"))
}
