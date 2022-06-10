package compiler.ext

import clangast.expr.*
import clangast.given
import clangast.stmt.{CExprStmt, CStmt}
import clangast.stubs.StdIOH
import clangast.types.{CCharType, CPointerType, CType}
import compiler.context.{IncludeTC, TranslationContext}
import compiler.CompilerCascade
import compiler.base.*

import scala.quoted.*

object CompileString extends TermPC with ApplyPC with TypePC {
  override def compileLiteral(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Literal, CExpr] = {
      import quotes.reflect.*
  
      {
        case Literal(StringConstant(x)) => CStringLiteral(x)
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

  private def compileApplyImpl(using Quotes)(using ctx: IncludeTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*
  
      {
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[Boolean] =>
          printf(
            "%s",
            CConditionalOperator(
              cascade.dispatch(_.compileTermToCExpr)(arg),
              CStringLiteral("true"),
              CStringLiteral("false")
            )
          )
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[Boolean] =>
          printf(
            "%s\\n",
            CConditionalOperator(
              cascade.dispatch(_.compileTermToCExpr)(arg),
              CStringLiteral("true"),
              CStringLiteral("false")
            )
          )
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[Byte | Short | Int | Long] =>
          printf("%d", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[Byte | Short | Int | Long] =>
          printf("%d\\n", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[Char] =>
          printf("%c", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[Char] =>
          printf("%c\\n", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[Float | Double] =>
          printf("%f", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[Float | Double] =>
          printf("%f\\n", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[String] =>
          printf("%s", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[String] =>
          printf("%s\\n", cascade.dispatch(_.compileTermToCExpr)(arg))
      }
    }

  override def compileApply(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[IncludeTC](compileApplyImpl)

  private def printf(format: String, arg: CExpr)(using IncludeTC): CExpr =
    CCallExpr(
      CDeclRefExpr(StdIOH.printf),
      List(CStringLiteral(format), arg)
    )
}
