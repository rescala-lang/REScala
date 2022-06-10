package compiler.ext

import clangast.expr.*
import clangast.{CASTNode, given}
import clangast.stmt.{CCompoundStmt, CExprStmt, CStmt}
import clangast.stubs.StdIOH
import clangast.types.{CCharType, CPointerType, CType}
import compiler.context.TranslationContext
import compiler.CompilerCascade
import compiler.base.*

import scala.quoted.*

object CompileString extends TermPC with TypePC with StringPC {
  override def compileLiteral(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Literal, CExpr] = {
      import quotes.reflect.*
  
      {
        case Literal(StringConstant(x)) => CStringLiteral(x)
      }
    }

  override def compileTerm(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Term, CASTNode] = {
      import quotes.reflect.*
  
      {
        case Apply(Ident("print"), List(arg)) =>
          cascade.dispatch(_.compilePrint)(
            cascade.dispatch(_.compileTermToCExpr)(arg),
            arg.tpe
          )
        case Apply(Ident("println"), List(arg)) =>
          CCompoundStmt(List(
            cascade.dispatch(_.compilePrint)(
              cascade.dispatch(_.compileTermToCExpr)(arg),
              arg.tpe
            ),
            printf("\\n")
          ))
      }
    }

  override def compileTypeRepr(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = {
      import quotes.reflect.*

      {
        case ConstantType(_: StringConstant) => CPointerType(CCharType)
        case tpe if tpe =:= TypeRepr.of[String] => CPointerType(CCharType)
      }
    }

  override def compilePrint(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = {
      import quotes.reflect.*

      {
        case (expr, tpe) if tpe <:< TypeRepr.of[Boolean] =>
          printf(
            "%s",
            CConditionalOperator(
              expr,
              CStringLiteral("true"),
              CStringLiteral("false")
            )
          )
        case (expr, tpe) if tpe <:< TypeRepr.of[Byte | Short | Int | Long] =>
          printf("%d", expr)
        case (expr, tpe) if tpe <:< TypeRepr.of[Char] =>
          printf("%c", expr)
        case (expr, tpe) if tpe <:< TypeRepr.of[Float | Double] =>
          printf("%f", expr)
        case (expr, tpe) if tpe <:< TypeRepr.of[String] =>
          printf("%s", expr)
      }
    }

  def printf(format: String, args: CExpr*)(using TranslationContext): CExpr =
    CCallExpr(
      CDeclRefExpr(StdIOH.printf),
      CStringLiteral(format) :: args.toList
    )
}
