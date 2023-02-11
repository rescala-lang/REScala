package compiler.base

import clangast.expr.CExpr
import clangast.types.CType
import compiler.{CompilerFragment, FragmentedCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait TypeIFFragment extends CompilerFragment {
  def compileTypeRepr(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CType] = PartialFunction.empty

  def typeName(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, String] = PartialFunction.empty

  def classTypeName(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, String] = PartialFunction.empty

  def defaultValue(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CExpr] = PartialFunction.empty
}
