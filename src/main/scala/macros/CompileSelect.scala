package macros

import clangast.expr.{CExpr, CMemberExpr, CParenExpr}
import clangast.expr.unaryop.*
import macros.ScalaToC.*
import macros.CompileTerm.compileTermToCExpr
import macros.CompileType.*

import scala.quoted.*

object CompileSelect {
  def compileSelect(using Quotes)(select: quotes.reflect.Select, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    // Select with function calls should already be handled by outer apply
    // This handles selecting fields on standard data structures

    val handleProduct = CompileProduct.compileSelect(ctx)

    select match {
      case handleProduct(expr) => expr
      case Select(qualifier, name) if canCompileToCUnaryOperator(qualifier, name) =>
        compileSelectToCUnaryOperator(select, ctx)
      case _ => throw new MatchError(select.show(using Printer.TreeStructure))
    }
  }

  def canCompileToCUnaryOperator(using Quotes)(term: quotes.reflect.Term, name: String): Boolean = {
    import quotes.reflect.*

    val unaryArithmeticOperators = List("unary_+", "unary_-")
    val unaryBitwiseOperators = List("unary_~")
    val unaryLogicalOperators = List("unary_!")

    val numberUnaryOperators = unaryArithmeticOperators ++ unaryBitwiseOperators
    val booleanUnaryOperators = unaryLogicalOperators

    isNumberType(term.tpe) && numberUnaryOperators.contains(name) ||
      term.tpe <:< TypeRepr.of[Boolean] && booleanUnaryOperators.contains(name)
  }

  def compileSelectToCUnaryOperator(using Quotes)(select: quotes.reflect.Select, ctx: TranslationContext): CUnaryOperator = {
    import quotes.reflect.*

    val Select(qualifier, name) = select

    val subExpr = compileTermToCExpr(qualifier, ctx)

    name match {
      case "unary_+" => CUnaryPlusExpr(CParenExpr(subExpr))
      case "unary_-" => CUnaryMinusExpr(CParenExpr(subExpr))
      case "unary_~" => CBitwiseNotExpr(CParenExpr(subExpr))
      case "unary_!" => CNotExpr(CParenExpr(subExpr))
      case _ => throw new MatchError(select.show(using Printer.TreeStructure))
    }
  }
}
