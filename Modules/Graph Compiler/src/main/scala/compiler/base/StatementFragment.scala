package compiler.base

import clangast.CASTNode
import clangast.stmt.{CDeclStmt, CStmt}
import compiler.context.TranslationContext
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch

import scala.quoted.*

object StatementFragment extends StatementIFFragment {
  override def compileStatement(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Statement, CASTNode] = {
    import quotes.reflect.*

    {
      case definition: Definition => dispatch[DefinitionIFFragment](_.compileDefinition)(definition)
      case term: Term             => dispatch[TermIFFragment](_.compileTerm)(term)
    }
  }

  override def compileStatementToCStmt(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Statement, CStmt] = {
    import quotes.reflect.*

    {
      case definition: Definition => CDeclStmt(dispatch[DefinitionIFFragment](_.compileDefinition)(definition))
      case term: Term             => dispatch[TermIFFragment](_.compileTermToCStmt)(term)
    }
  }
}
