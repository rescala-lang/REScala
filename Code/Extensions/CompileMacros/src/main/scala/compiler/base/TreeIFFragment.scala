package compiler.base

import clangast.CASTNode
import compiler.{CompilerFragment, FragmentedCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait TreeIFFragment extends CompilerFragment {
  def compileTree(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Tree, CASTNode] = PartialFunction.empty
}
