package compiler.base

import clangast.expr.CExpr
import clangast.types.CType
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait TypePC extends PartialCompiler {
  def compileTypeRepr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = PartialFunction.empty

  def typeName(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = PartialFunction.empty

  def classTypeName(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = PartialFunction.empty
  
  def defaultValue(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CExpr] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileTypeRepr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = PartialCompiler.ensurePC[TypePC](p, _.compileTypeRepr)

  def typeName(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = PartialCompiler.ensurePC[TypePC](p, _.typeName)

  def classTypeName(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = PartialCompiler.ensurePC[TypePC](p, _.classTypeName)

  def defaultValue(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CExpr] = PartialCompiler.ensurePC[TypePC](p, _.defaultValue)
}
