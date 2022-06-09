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

  def compileApply(using Quotes)(using ctx: IncludeTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*
  
      {
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[Boolean] =>
          ctx.includes.add(StdIOH.include)
          printf(
            "%s",
            CConditionalOperator(
              cascade.dispatch(_.compileTermToCExpr)(arg),
              CStringLiteral("true"),
              CStringLiteral("false")
            )
          )
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[Boolean] =>
          ctx.includes.add(StdIOH.include)
          printf(
            "%s\\n",
            CConditionalOperator(
              cascade.dispatch(_.compileTermToCExpr)(arg),
              CStringLiteral("true"),
              CStringLiteral("false")
            )
          )
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[Byte | Short | Int | Long] =>
          ctx.includes.add(StdIOH.include)
          printf("%d", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[Byte | Short | Int | Long] =>
          ctx.includes.add(StdIOH.include)
          printf("%d\\n", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[Char] =>
          ctx.includes.add(StdIOH.include)
          printf("%c", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[Char] =>
          ctx.includes.add(StdIOH.include)
          printf("%c\\n", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[Float | Double] =>
          ctx.includes.add(StdIOH.include)
          printf("%f", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[Float | Double] =>
          ctx.includes.add(StdIOH.include)
          printf("%f\\n", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[String] =>
          ctx.includes.add(StdIOH.include)
          printf("%s", cascade.dispatch(_.compileTermToCExpr)(arg))
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[String] =>
          ctx.includes.add(StdIOH.include)
          printf("%s\\n", cascade.dispatch(_.compileTermToCExpr)(arg))
      }
    }

  override def compileApply(using q: Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = ctx match {
      case c: IncludeTC => compileApply(using q)(using c, cascade)
    }

  private def printf(format: String, arg: CExpr): CExpr =
    CCallExpr(
      CDeclRefExpr(StdIOH.printf),
      List(CStringLiteral(format), arg)
    )
}
