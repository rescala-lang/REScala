package clangast.stubs

import clangast.given
import clangast.decl.{CFunctionDecl, CInclude, CVarDecl}
import clangast.expr.CIntegerLiteral
import clangast.types.{CIntegerType, CType, CTypedefType}
import compiler.context.TranslationContext

object HashmapH extends CLibraryStub {
  override val include: CInclude = CInclude("hashmap.h", true)

  def map_t(using ctx: TranslationContext): CType = includeStub(CTypedefType("map_t"))
  def any_t(using ctx: TranslationContext): CType = includeStub(CTypedefType("any_t"))

  def MAP_MISSING(using ctx: TranslationContext): CVarDecl =
    includeStub(CVarDecl("MAP_MISSING", CIntegerType, Some(CIntegerLiteral(-3))))
  def MAP_FULL(using ctx: TranslationContext): CVarDecl =
    includeStub(CVarDecl("MAP_FULL", CIntegerType, Some(CIntegerLiteral(-2))))
  def MAP_OMEM(using ctx: TranslationContext): CVarDecl =
    includeStub(CVarDecl("MAP_OMEM", CIntegerType, Some(CIntegerLiteral(-1))))
  def MAP_OK(using ctx: TranslationContext): CVarDecl =
    includeStub(CVarDecl("MAP_OK", CIntegerType, Some(CIntegerLiteral(0))))

  def hashmap_new(using ctx: TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("hashmap_new"))
  def hashmap_iterate(using ctx: TranslationContext): CFunctionDecl = includeStub(CFunctionStub("hashmap_iterate"))
  def hashmap_put(using ctx: TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("hashmap_put"))
  def hashmap_get(using ctx: TranslationContext): CFunctionDecl     = includeStub(CFunctionStub("hashmap_get"))
  def hashmap_remove(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("hashmap_remove"))
  def hashmap_get_one(using ctx: TranslationContext): CFunctionDecl = includeStub(CFunctionStub("hashmap_get_one"))
  def hashmap_free(using ctx: TranslationContext): CFunctionDecl    = includeStub(CFunctionStub("hashmap_free"))
  def hashmap_length(using ctx: TranslationContext): CFunctionDecl  = includeStub(CFunctionStub("hashmap_length"))
}
