package macros

import clangast.expr.*
import clangast.expr.binaryop.*
import clangast.expr.unaryop.CNotExpr
import macros.ScalaToC.*
import macros.CompileTerm.compileTermToCExpr
import macros.CompileType.*

import scala.quoted.*

object CompileApply {
  def compileApply(using Quotes)(apply: quotes.reflect.Apply, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    // There is no general case here, instead just multiple special cases for different types of functions applied
    // such as those that can be translated to operator and methods on standard data structures that need to be given
    // as C code

    apply match {
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
      case Apply(Select(_, "apply"), l) if isProductApply(apply) =>
        CCallExpr(CDeclRefExpr(getRecordCreator(apply.tpe, ctx)), l.map(compileTermToCExpr(_, ctx)))
      case Apply(TypeApply(Select(_, "apply"), _), l) if isProductApply(apply) =>
        CCallExpr(CDeclRefExpr(getRecordCreator(apply.tpe, ctx)), l.map(compileTermToCExpr(_, ctx)))
      case Apply(Select(left, "=="), List(right)) if left.tpe <:< TypeRepr.of[Product] =>
        CCallExpr(
          CDeclRefExpr(getRecordEquals(left.tpe, ctx)),
          List(
            compileTermToCExpr(left, ctx),
            compileTermToCExpr(right, ctx)
          )
        )
      case Apply(Select(left, "!="), List(right)) if left.tpe <:< TypeRepr.of[Product] =>
        CNotExpr(
          CCallExpr(
            CDeclRefExpr(getRecordEquals(left.tpe, ctx)),
            List(
              compileTermToCExpr(left, ctx),
              compileTermToCExpr(right, ctx)
            )
          )
        )
      case Apply(Select(qualifier, name), List(_)) if canCompileToCBinaryOperator(qualifier, name) =>
        compileApplyToCBinaryOperator(apply, ctx)
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

  def isProductApply(using Quotes)(apply: quotes.reflect.Apply): Boolean = {
    import quotes.reflect.*

    apply match {
      case Apply(Select(i, "apply"), _) =>
        (apply.tpe <:< TypeRepr.of[Product]) && (apply.tpe.classSymbol.get == i.tpe.classSymbol.get.companionClass)
      case Apply(TypeApply(Select(i, "apply"), _), _) =>
        (apply.tpe <:< TypeRepr.of[Product]) && (apply.tpe.classSymbol.get == i.tpe.classSymbol.get.companionClass)
      case _ => false
    }
  }
}
