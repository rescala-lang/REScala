package clangast.stubs

import clangast.decl.{CFunctionDecl, CInclude}
import compiler.context.TranslationContext

object StringH extends CLibraryStub {
  override val include: CInclude = CInclude("string.h")

  def memchr(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("memchr"))
  def memcmp(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("memcmp"))
  def memcpy(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("memcpy"))
  def memmove(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("memmove"))
  def memset(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("memset"))
  def strcat(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("strcat"))
  def strncat(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("strncat"))
  def strchr(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("strchr"))
  def strcmp(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("strcmp"))
  def strncmp(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("strncmp"))
  def strcoll(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("strcoll"))
  def strcpy(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("strcpy"))
  def strncpy(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("strncpy"))
  def strcspn(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("strcspn"))
  def strerror(using TranslationContext): CFunctionDecl = includeStub(CFunctionStub("strerror"))
  def strlen(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("strlen"))
  def strpbrk(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("strpbrk"))
  def strrchr(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("strrchr"))
  def strspn(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("strspn"))
  def strstr(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("strstr"))
  def strtok(using TranslationContext): CFunctionDecl   = includeStub(CFunctionStub("strtok"))
  def strxfrm(using TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("strxfrm"))
}
