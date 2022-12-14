package compiler.ext

import clangast.{CASTNode, given}
import clangast.expr.CExpr
import clangast.stmt.CStmt
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.base.*
import compiler.base.ApplyFragment.varArgs
import compiler.context.TranslationContext

import scala.quoted.*

object MainFunctionFragment extends TermIFFragment {
  override def compileTerm(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Term, CASTNode] = {
    import quotes.reflect.*

    {
      case Apply(Select(Ident("CMainFunction"), "startTransaction"), varArgs(assignments)) =>
        CTransactionStatement(assignments.map(compileSourceAssignment))
    }
  }

  private def compileSourceAssignment(using Quotes)(assignment: quotes.reflect.Term)(using FragmentedCompiler)(using
      TranslationContext
  ): (String, CExpr) = {
    import quotes.reflect.*

    assignment match {
      case Apply(Select(Ident(sourceName), ":="), List(v)) =>
        (sourceName, dispatch[TermIFFragment](_.compileTermToCExpr)(v))
    }
  }
}
