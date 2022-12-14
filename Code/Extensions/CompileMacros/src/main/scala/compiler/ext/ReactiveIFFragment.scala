package compiler.ext

import api2.CompiledReactive
import clangast.decl.CFunctionDecl
import compiler.{CompilerFragment, FragmentedCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait ReactiveIFFragment extends CompilerFragment {
  def compileReactiveTopLevelStmt(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Statement, Unit] = PartialFunction.empty

  def compileReactive(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Term, CompiledReactive] = PartialFunction.empty

  def compileReactiveExpr(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Term, CFunctionDecl] = PartialFunction.empty
}
