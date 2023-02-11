package compiler.base

import clangast.decl.{CDecl, CFunctionDecl, CParmVarDecl, CVarDecl}
import compiler.{CompilerFragment, FragmentedCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait DefinitionIFFragment extends CompilerFragment {
  def compileDefinition(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Definition, CDecl] = PartialFunction.empty

  def compileDefDef(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.DefDef, CFunctionDecl] = PartialFunction.empty

  def compileValDefToCVarDecl(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.ValDef, CVarDecl] = PartialFunction.empty

  def compileValDefToCParmVarDecl(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.ValDef, CParmVarDecl] = PartialFunction.empty
}
