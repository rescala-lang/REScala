package compiler.base

import clangast.given
import clangast.decl.CVarDecl
import clangast.expr.binaryop.*
import clangast.expr.*
import clangast.stmt.CCompoundStmt
import clangast.stubs.StdIOH
import compiler.context.TranslationContext
import compiler.CompilerCascade

import scala.quoted.*

object CompileApply extends ApplyPC {
  override def compileApply(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*
    
      {
        case Apply(Select(Select(Ident("math"), "package"), "max"), List(arg1, arg2)) =>
          val v1 = CVarDecl("_v1", cascade.dispatch(_.compileTypeRepr)(arg1.tpe), Some(cascade.dispatch(_.compileTermToCExpr)(arg1)))
          val v2 = CVarDecl("_v2", cascade.dispatch(_.compileTypeRepr)(arg2.tpe), Some(cascade.dispatch(_.compileTermToCExpr)(arg2)))

          val cond = CConditionalOperator(
            CGreaterThanExpr(CDeclRefExpr(v1), CDeclRefExpr(v2)),
            CDeclRefExpr(v1),
            CDeclRefExpr(v2)
          )
          
          CStmtExpr(CCompoundStmt(List(
            v1,
            v2,
            cond
          )))
        case apply@Apply(Select(qualifier, name), List(_)) if canCompileToCBinaryOperator(qualifier, name) =>
          compileApplyToCBinaryOperator(apply)
        case Apply(inner, List(Apply(TypeApply(Select(Ident("ClassTag"), "apply"), _), _))) =>
          cascade.dispatch(_.compileTermToCExpr)(inner)
        case Apply(inner, List(Select(Apply(TypeApply(Select(Ident("ClassTag"), "apply"), _), _), "wrap"))) =>
          // assume that this ClassTag magic can be ignored for our purposes
          cascade.dispatch(_.compileTermToCExpr)(inner)
      }
    }

  private def canCompileToCBinaryOperator(using Quotes)(term: quotes.reflect.Term, name: String): Boolean = {
    import quotes.reflect.*

    val binaryArithmeticOperators = List("+", "-", "*", "/", "%")
    val binaryBitwiseLogicalOperators = List("|", "&", "^")
    val binaryBitwiseOperators = binaryBitwiseLogicalOperators ++ List("<<", ">>", ">>>")
    val equalityOperators = List("==", "!=")
    val relationalOperators = equalityOperators ++ List("<", "<=", ">", ">=")
    val binaryLogicalOperators = List("&&", "||")

    val numberBinaryOperators = binaryArithmeticOperators ++ binaryBitwiseOperators ++ relationalOperators
    val booleanBinaryOperators = binaryBitwiseLogicalOperators ++ equalityOperators ++ binaryLogicalOperators

    CompileType.isNumberType(term.tpe) && numberBinaryOperators.contains(name) ||
      term.tpe <:< TypeRepr.of[Boolean] && booleanBinaryOperators.contains(name)
  }

  private def compileApplyToCBinaryOperator(using Quotes)(apply: quotes.reflect.Apply)(using ctx: TranslationContext, cascade: CompilerCascade): CParenExpr = {
    import quotes.reflect.*

    val Apply(Select(qualifier, name), List(arg)) = apply

    val lhs = cascade.dispatch(_.compileTermToCExpr)(qualifier)
    val rhs = cascade.dispatch(_.compileTermToCExpr)(arg)

    name match {
      case "+" if CompileType.isNumberType(arg.tpe) => CParenExpr(CPlusExpr(lhs, rhs))
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
