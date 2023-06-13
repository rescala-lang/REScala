package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}
import compiler.context.TranslationContext

object StdLibH extends CLibraryStub {
  override val include: CInclude = CInclude("stdlib.h")

  def atof(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("atof"))
  def atoi(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("atoi"))
  def atol(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("atol"))
  def strtod(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("strtod"))
  def strtol(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("strtol"))
  def strtoul(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("strtoul"))
  def calloc(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("calloc"))
  def free(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("free"))
  def malloc(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("malloc"))
  def realloc(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("realloc"))
  def abort(using TranslationContext): CFunctionDecl    = includeStub(CFunctionStub("abort"))
  def atexit(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("atexit"))
  def exit(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("exit"))
  def getenv(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("getenv"))
  def system(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("system"))
  def bsearch(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("bsearch"))
  def qsort(using TranslationContext): CFunctionDecl    = includeStub(CFunctionStub("qsort"))
  def abs(using TranslationContext): CFunctionDecl      = includeStub(CFunctionStub("abs"))
  def div(using TranslationContext): CFunctionDecl      = includeStub(CFunctionStub("div"))
  def labs(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("labs"))
  def ldiv(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("ldiv"))
  def rand(using TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("rand"))
  def srand(using TranslationContext): CFunctionDecl    = includeStub(CFunctionStub("srand"))
  def mblen(using TranslationContext): CFunctionDecl    = includeStub(CFunctionStub("mblen"))
  def mbstowcs(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("mbstowcs"))
  def mbtowc(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("mbtowc"))
  def wcstombs(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("wcstombs"))
  def wctomb(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("wctomb"))
}
