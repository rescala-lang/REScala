package compiler.base

import clangast.expr.CExpr
import compiler.{CompilerFragment, FragmentedCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait RefIFFragment extends CompilerFragment {
  def compileRef(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Ref, CExpr] = PartialFunction.empty

  def compileIdent(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Ident, CExpr] = PartialFunction.empty
}
