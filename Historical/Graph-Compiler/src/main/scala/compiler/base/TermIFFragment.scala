package compiler.base

import clangast.CASTNode
import clangast.expr.*
import clangast.stmt.*
import compiler.{CompilerFragment, FragmentedCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait TermIFFragment extends CompilerFragment {
  def compileTerm(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Term, CASTNode] = PartialFunction.empty

  def compileTermToCStmt(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Term, CStmt] = PartialFunction.empty

  def compileTermToCExpr(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Term, CExpr] = PartialFunction.empty

  def compileLiteral(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Literal, CExpr] = PartialFunction.empty

  def compileAssign(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Assign, CExpr] = PartialFunction.empty

  def compileBlockToCStmtExpr(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Block, CStmtExpr] = PartialFunction.empty

  def compileBlockToCCompoundStmt(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Block, CCompoundStmt] = PartialFunction.empty

  def compileBlockToFunctionBody(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Block, CCompoundStmt] = PartialFunction.empty

  def compileIfToCIfStmt(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.If, CIfStmt] = PartialFunction.empty

  def compileIfToCConditionalOperator(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.If, CConditionalOperator] = PartialFunction.empty

  def compileReturn(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Return, CReturnStmt] = PartialFunction.empty

  def compileWhile(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.While, CWhileStmt] = PartialFunction.empty
}
