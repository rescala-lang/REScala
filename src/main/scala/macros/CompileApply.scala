package macros

import clangast.decl.CFunctionDecl
import clangast.expr.*
import clangast.expr.binaryop.*
import clangast.expr.unaryop.CNotExpr
import macros.ScalaToC.*
import macros.CompileTerm.compileTermToCExpr
import macros.CompileType.*
import macros.CompileDefinition.compileDefDef

import scala.quoted.*

object CompileApply {
  def compileApply(using Quotes)(apply: quotes.reflect.Apply, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    // There is no general case here, instead just multiple special cases for different types of functions applied
    // such as those that can be translated to operator and methods on standard data structures that need to be given
    // as C code

    val handleProduct = CompileProduct.compileApply(ctx)
    val handleArray = CompileArray.compileApply(ctx)
    
    apply match {
      case handleProduct(expr) => expr
      case handleArray(expr) => expr
      case Apply(Ident("println"), List(arg)) =>
        // needs unlinked CCallStmt (name of function instead of reference)
        // alternatively, provide mock definitions for library functions
        compileTermToCExpr(arg, ctx)
      case Apply(Select(Select(Ident("math"), "package"), "max"), List(arg1, arg2)) =>
        // Should compile to StmtExpr instead where arg1 and arg2 are only evaluated once each
        // Needs translation of types

        val expr1 = compileTermToCExpr(arg1, ctx)
        val expr2 = compileTermToCExpr(arg2, ctx)

        CConditionalOperator(
          CGreaterThanExpr(expr1, expr2),
          expr1,
          expr2
        )
      case Apply(Select(qualifier, name), List(_)) if canCompileToCBinaryOperator(qualifier, name) =>
        compileApplyToCBinaryOperator(apply, ctx)
      case Apply(inner, List(Select(Apply(TypeApply(Select(Ident("ClassTag"), "apply"), _), _), "wrap"))) =>
        // assume that this ClassTag magic can be ignored for our purposes
        compileTermToCExpr(inner, ctx)
      case _ => throw new MatchError(apply.show(using Printer.TreeStructure))
    }
  }

  def canCompileToCBinaryOperator(using Quotes)(term: quotes.reflect.Term, name: String): Boolean = {
    import quotes.reflect.*

    val binaryArithmeticOperators = List("+", "-", "*", "/", "%")
    val binaryBitwiseLogicalOperators = List("|", "&", "^")
    val binaryBitwiseOperators = binaryBitwiseLogicalOperators ++ List("<<", ">>", ">>>")
    val equalityOperators = List("==", "!=")
    val relationalOperators = equalityOperators ++ List("<", "<=", ">", ">=")
    val binaryLogicalOperators = List("&&", "||")

    val numberBinaryOperators = binaryArithmeticOperators ++ binaryBitwiseOperators ++ relationalOperators
    val booleanBinaryOperators = binaryBitwiseLogicalOperators ++ equalityOperators ++ binaryLogicalOperators

    isNumberType(term.tpe) && numberBinaryOperators.contains(name) ||
      term.tpe <:< TypeRepr.of[Boolean] && booleanBinaryOperators.contains(name)
  }

  def compileApplyToCBinaryOperator(using Quotes)(apply: quotes.reflect.Apply, ctx: TranslationContext): CParenExpr = {
    import quotes.reflect.*

    val Apply(Select(qualifier, name), List(arg)) = apply

    val lhs = compileTermToCExpr(qualifier, ctx)
    val rhs = compileTermToCExpr(arg, ctx)

    name match {
      case "+" if isNumberType(arg.tpe) => CParenExpr(CPlusExpr(lhs, rhs))
      case "-" => CParenExpr(CMinusExpr(lhs, rhs))
      case "*" => CParenExpr(CProdExpr(lhs, rhs))
      case "/" => CParenExpr(CDivExpr(lhs, rhs))
      case "%" => CParenExpr(CModExpr(lhs, rhs))
      case "|" => CParenExpr(CBitwiseOrExpr(lhs, rhs))
      case "&" => CParenExpr(CBitwiseAndExpr(lhs, rhs))
      case "^" => CParenExpr(CBitwiseXorExpr(lhs, rhs))
      case "<<" => CParenExpr(CLeftShiftExpr(lhs, rhs))
      case ">>" => CParenExpr(CRightShiftExpr(lhs, rhs))
      case ">>>" => throw new MatchError(apply.show(using Printer.TreeStructure))
      case "==" => CParenExpr(CEqualsExpr(lhs, rhs))
      case "!=" => CParenExpr(CNotEqualsExpr(lhs, rhs))
      case "<" => CParenExpr(CLessThanExpr(lhs, rhs))
      case "<=" => CParenExpr(CLessEqualsExpr(lhs, rhs))
      case ">" => CParenExpr(CGreaterThanExpr(lhs, rhs))
      case ">=" => CParenExpr(CGreaterEqualsExpr(lhs, rhs))
      case _ => throw new MatchError(apply.show(using Printer.TreeStructure))
    }
  }

  def varArgs(using Quotes): PartialFunction[List[quotes.reflect.Term], List[quotes.reflect.Term]] = args => {
    import quotes.reflect.*

    args match {
      case List(x, Typed(Repeated(xs, _), _)) => x :: xs
      case List(Typed(Repeated(xs, _), _)) => xs
    }
  }
}
