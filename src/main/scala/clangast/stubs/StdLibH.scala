package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}
import clangast.types.CVoidType
import compiler.context.IncludeTC

object StdLibH extends CLibraryStub {
  override val include: CInclude = CInclude("stdlib.h")
  
  def atof(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("atof"))
  def atoi(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("atoi"))
  def atol(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("atol"))
  def strtod(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strtod"))
  def strtol(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strtol"))
  def strtoul(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strtoul"))
  def calloc(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("calloc"))
  def free(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("free"))
  def malloc(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("malloc"))
  def realloc(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("realloc"))
  def abort(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("abort"))
  def atexit(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("atexit"))
  def exit(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("exit"))
  def getenv(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("getenv"))
  def system(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("system"))
  def bsearch(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("bsearch"))
  def qsort(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("qsort"))
  def abs(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("abs"))
  def div(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("div"))
  def labs(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("labs"))
  def ldiv(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("ldiv"))
  def rand(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("rand"))
  def srand(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("srand"))
  def mblen(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("mblen"))
  def mbstowcs(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("mbstowcs"))
  def mbtowc(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("mbtowc"))
  def wcstombs(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("wcstombs"))
  def wctomb(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("wctomb"))
}
