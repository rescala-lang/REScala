package compiler.base

import clangast.expr.CExpr
import compiler.{CompilerFragment, FragmentedCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait SelectIFFragment extends CompilerFragment {
  def compileSelect(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Select, CExpr] = PartialFunction.empty
}
