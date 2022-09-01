package compiler.ext

import clangast.decl.CFunctionDecl
import compiler.context.TranslationContext
import compiler.{CompilerCascade, PartialCompiler}

import scala.quoted.*

trait SerializationPC extends PartialCompiler {
  protected val SERIALIZE = "SERIALIZE"
  protected val DESERIALIZE = "DESERIALIZE"

  def compileSerialize(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialFunction.empty

  def compileDeserialize(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialFunction.empty

  def serializationRetainsEquality(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileSerialize(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialCompiler.ensurePC[SerializationPC](p, _.compileSerialize)

  def compileDeserialize(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialCompiler.ensurePC[SerializationPC](p, _.compileDeserialize)

  def serializationRetainsEquality(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = PartialCompiler.ensurePC[SerializationPC](p, _.serializationRetainsEquality)
}
