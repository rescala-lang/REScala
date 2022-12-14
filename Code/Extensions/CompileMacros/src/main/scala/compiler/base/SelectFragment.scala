package compiler.base

import clangast.expr.unaryop.*
import clangast.expr.{CExpr, CParenExpr}
import compiler.context.TranslationContext
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch

import scala.quoted.*

object SelectFragment extends SelectIFFragment {
  override def compileSelect(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Select, CExpr] = {
    import quotes.reflect.*

    {
      case select @ Select(qualifier, name) if canCompileToCUnaryOperator(qualifier, name) =>
        compileSelectToCUnaryOperator(select)
    }
  }

  private def canCompileToCUnaryOperator(using Quotes)(term: quotes.reflect.Term, name: String): Boolean = {
    import quotes.reflect.*

    val unaryArithmeticOperators = List("unary_+", "unary_-")
    val unaryBitwiseOperators    = List("unary_~")
    val unaryLogicalOperators    = List("unary_!")

    val numberUnaryOperators  = unaryArithmeticOperators ++ unaryBitwiseOperators
    val booleanUnaryOperators = unaryLogicalOperators

    TypeFragment.isNumberType(term.tpe) && numberUnaryOperators.contains(name) ||
    term.tpe <:< TypeRepr.of[Boolean] && booleanUnaryOperators.contains(name)
  }

  private def compileSelectToCUnaryOperator(using Quotes)(select: quotes.reflect.Select)(using FragmentedCompiler)(using
      TranslationContext
  ): CUnaryOperator = {
    import quotes.reflect.*

    val Select(qualifier, name) = select

    val subExpr = dispatch[TermIFFragment](_.compileTermToCExpr)(qualifier)

    name match {
      case "unary_+" => CUnaryPlusExpr(CParenExpr(subExpr))
      case "unary_-" => CUnaryMinusExpr(CParenExpr(subExpr))
      case "unary_~" => CBitwiseNotExpr(CParenExpr(subExpr))
      case "unary_!" => CNotExpr(CParenExpr(subExpr))
      case _         => throw new MatchError(select.show(using Printer.TreeStructure))
    }
  }
}
