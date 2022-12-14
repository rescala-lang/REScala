package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}
import compiler.context.TranslationContext

object MathH extends CLibraryStub {
  override val include: CInclude = CInclude("math.h")

  def acos(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("acos"))
  def asin(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("asin"))
  def atan(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("atan"))
  def atan2(using ctx: TranslationContext): CFunctionDecl = includeStub(CFunctionStub("atan2"))
  def cos(using ctx: TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("cos"))
  def cosh(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("cosh"))
  def sin(using ctx: TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("sin"))
  def sinh(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("sinh"))
  def tanh(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("tanh"))
  def exp(using ctx: TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("exp"))
  def frexp(using ctx: TranslationContext): CFunctionDecl = includeStub(CFunctionStub("frexp"))
  def ldexp(using ctx: TranslationContext): CFunctionDecl = includeStub(CFunctionStub("ldexp"))
  def log(using ctx: TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("log"))
  def log10(using ctx: TranslationContext): CFunctionDecl = includeStub(CFunctionStub("log10"))
  def modf(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("modf"))
  def pow(using ctx: TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("pow"))
  def sqrt(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("sqrt"))
  def ceil(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("ceil"))
  def fabs(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("fabs"))
  def floor(using ctx: TranslationContext): CFunctionDecl = includeStub(CFunctionStub("floor"))
  def fmod(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("fmod"))
}
