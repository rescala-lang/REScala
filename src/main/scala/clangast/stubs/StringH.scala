package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}
import compiler.context.IncludeTC

object StringH extends CLibraryStub {
  override val include: CInclude = CInclude("string.h")
  
  def memchr(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("memchr"))
  def memcmp(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("memcmp"))
  def memcpy(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("memcpy"))
  def memmove(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("memmove"))
  def memset(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("memset"))
  def strcat(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strcat"))
  def strncat(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strncat"))
  def strchr(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strchr"))
  def strcmp(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strcmp"))
  def strncmp(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strncmp"))
  def strcoll(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strcoll"))
  def strcpy(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strcpy"))
  def strncpy(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strncpy"))
  def strcspn(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strcspn"))
  def strerror(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strerror"))
  def strlen(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strlen"))
  def strpbrk(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strpbrk"))
  def strrchr(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strrchr"))
  def strspn(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strspn"))
  def strstr(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strstr"))
  def strtok(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strtok"))
  def strxfrm(using IncludeTC): CFunctionDecl = includeStub(CFunctionStub("strxfrm"))
}
