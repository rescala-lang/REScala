package compiler.base

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
}

extension (p: PartialCompiler) {
  def compileTypeRepr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = p match {
      case typePC: TypePC => typePC.compileTypeRepr
      case _ => PartialFunction.empty
    }

  def typeName(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = p match {
      case typePC: TypePC => typePC.typeName
      case _ => PartialFunction.empty
    }

  def classTypeName(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, String] = p match {
      case typePC: TypePC => typePC.classTypeName
      case _ => PartialFunction.empty
    }
}
