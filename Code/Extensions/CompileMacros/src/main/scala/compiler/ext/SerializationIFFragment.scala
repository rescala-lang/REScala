package compiler.ext

import clangast.decl.CFunctionDecl
import compiler.context.TranslationContext
import compiler.{CompilerFragment, FragmentedCompiler}

import scala.quoted.*

trait SerializationIFFragment extends CompilerFragment {
  protected val SERIALIZE   = "SERIALIZE"
  protected val DESERIALIZE = "DESERIALIZE"

  def compileSerialize(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialFunction.empty

  def compileDeserialize(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialFunction.empty

  def serializationRetainsEquality(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, Boolean] = PartialFunction.empty
}
