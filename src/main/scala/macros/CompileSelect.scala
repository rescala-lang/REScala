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
    // Check that this select is either on a case class or tuple
    // compile to a MemberExpr using a struct definition from the ctx
    // if no struct definition exists yet, create a new one

    val Select(qualifier, name) = select

    if (canCompileToCUnaryOperator(qualifier, name)) {
      compileSelectToCUnaryOperator(select, ctx)
    } else if (isProductFieldAccess(qualifier, name)) {
      val recordDecl = getRecordDecl(qualifier.tpe, ctx)

      // When is the arrow necessary?
      CMemberExpr(compileTermToCExpr(qualifier, ctx), recordDecl.fields.find(_.name.equals(name)).get)
    } else {
      throw new MatchError(select.show(using Printer.TreeStructure))
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

  def isProductFieldAccess(using Quotes)(term: quotes.reflect.Term, name: String): Boolean = {
    import quotes.reflect.*

    (term.tpe <:< TypeRepr.of[Product]) && term.tpe.classSymbol.get.caseFields.exists(_.name.equals(name))
  }
}
