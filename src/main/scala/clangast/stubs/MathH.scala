package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}
import compiler.context.IncludeTC

object MathH extends CLibraryStub {
  override val include: CInclude = CInclude("math.h")
  
  def acos(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("acos"))
  def asin(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("asin"))
  def atan(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("atan"))
  def atan2(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("atan2"))
  def cos(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("cos"))
  def cosh(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("cosh"))
  def sin(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("sin"))
  def sinh(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("sinh"))
  def tanh(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("tanh"))
  def exp(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("exp"))
  def frexp(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("frexp"))
  def ldexp(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("ldexp"))
  def log(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("log"))
  def log10(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("log10"))
  def modf(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("modf"))
  def pow(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("pow"))
  def sqrt(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("sqrt"))
  def ceil(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("ceil"))
  def fabs(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("fabs"))
  def floor(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("floor"))
  def fmod(using ctx: IncludeTC): CFunctionDecl = includeStub(CFunctionStub("fmod"))
}
