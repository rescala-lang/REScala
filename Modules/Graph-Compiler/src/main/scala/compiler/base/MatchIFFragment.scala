package compiler.base

import clangast.decl.CVarDecl
import clangast.expr.CExpr
import clangast.stmt.CStmt
import compiler.context.TranslationContext
import compiler.{CompilerFragment, FragmentedCompiler}

import scala.quoted.*

trait MatchIFFragment extends CompilerFragment {
  def compileMatchToCStmt(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Match, CStmt] = PartialFunction.empty

  def compileMatchToCExpr(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Match, CExpr] = PartialFunction.empty

  def compileCaseDef(using Quotes)(using FragmentedCompiler)(using TranslationContext): PartialFunction[
    (quotes.reflect.CaseDef, CExpr, quotes.reflect.TypeRepr),
    (Option[CExpr], List[CVarDecl], List[CStmt])
  ] = PartialFunction.empty

  def compilePattern(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] =
    PartialFunction.empty
}
