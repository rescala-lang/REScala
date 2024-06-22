package compiler.base

import clangast.CASTNode
import clangast.stmt.CStmt
import compiler.{CompilerFragment, FragmentedCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait StatementIFFragment extends CompilerFragment {
  def compileStatement(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Statement, CASTNode] = PartialFunction.empty

  def compileStatementToCStmt(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Statement, CStmt] = PartialFunction.empty
}
