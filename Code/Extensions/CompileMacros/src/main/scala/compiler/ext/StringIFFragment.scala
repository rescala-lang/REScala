package compiler.ext

import clangast.expr.CExpr
import clangast.stmt.CStmt
import compiler.{CompilerFragment, FragmentedCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait StringIFFragment extends CompilerFragment {
  def compilePrint(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = PartialFunction.empty

  def compileToString(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = PartialFunction.empty
}
