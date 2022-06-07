package compiler.ext

import clangast.expr.{CCallExpr, CConditionalOperator, CDeclRefExpr, CExpr, CStringLiteral}
import clangast.given
import clangast.stmt.{CExprStmt, CStmt}
import clangast.stubs.StdIOH
import clangast.types.{CCharType, CPointerType, CType}
import compiler.{CompilerCascade, PartialCompiler, TranslationContext}

import scala.quoted.*

object CompileString extends PartialCompiler {
  override def compileTypeRepr(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = {
      import quotes.reflect.*

      {
        case ConstantType(_: StringConstant) => CPointerType(CCharType)
        case tpe if tpe =:= TypeRepr.of[String] => CPointerType(CCharType)
      }
    }

  override def compileLiteral(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Literal, CExpr] = {
      import quotes.reflect.*

      {
        case Literal(StringConstant(x)) => CStringLiteral(x)
      }
    }

  private def printf(format: String, arg: CExpr): CExpr =
    CCallExpr(
      CDeclRefExpr(StdIOH.printf),
      List(CStringLiteral(format), arg)
    )

  override def compileApply(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*

      {
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[Boolean] =>
          ctx.includes.add(StdIOH.include)
          printf(
            "%s",
            CConditionalOperator(
              cascade.compileTermToCExpr(arg),
              CStringLiteral("true"),
              CStringLiteral("false")
            )
          )
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[Boolean] =>
          ctx.includes.add(StdIOH.include)
          printf(
            "%s\\n",
            CConditionalOperator(
              cascade.compileTermToCExpr(arg),
              CStringLiteral("true"),
              CStringLiteral("false")
            )
          )
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[Byte | Short | Int | Long] =>
          ctx.includes.add(StdIOH.include)
          printf("%d", cascade.compileTermToCExpr(arg))
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[Byte | Short | Int | Long] =>
          ctx.includes.add(StdIOH.include)
          printf("%d\\n", cascade.compileTermToCExpr(arg))
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[Char] =>
          ctx.includes.add(StdIOH.include)
          printf("%c", cascade.compileTermToCExpr(arg))
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[Char] =>
          ctx.includes.add(StdIOH.include)
          printf("%c\\n", cascade.compileTermToCExpr(arg))
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[Float | Double] =>
          ctx.includes.add(StdIOH.include)
          printf("%f", cascade.compileTermToCExpr(arg))
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[Float | Double] =>
          ctx.includes.add(StdIOH.include)
          printf("%f\\n", cascade.compileTermToCExpr(arg))
        case Apply(Ident("print"), List(arg)) if arg.tpe <:< TypeRepr.of[String] =>
          ctx.includes.add(StdIOH.include)
          printf("%s", cascade.compileTermToCExpr(arg))
        case Apply(Ident("println"), List(arg)) if arg.tpe <:< TypeRepr.of[String] =>
          ctx.includes.add(StdIOH.include)
          printf("%s\\n", cascade.compileTermToCExpr(arg))
      }
    }
}
