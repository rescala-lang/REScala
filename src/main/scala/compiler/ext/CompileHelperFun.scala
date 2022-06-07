package compiler.ext

import clangast.expr.{CCallExpr, CExpr, CUnlinkedCallExpr}
import compiler.{CHelperFun, CompilerCascade, PartialCompiler, TranslationContext}

import scala.quoted.*

object CompileHelperFun extends PartialCompiler {
  override def compileApply(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*

      {
        case Apply(Select(helper@Ident(name), "apply"), args) if helper.tpe <:< TypeRepr.of[CHelperFun] =>
          CUnlinkedCallExpr(
            name,
            args.map(cascade.compileTermToCExpr)
          )
      }
    }
}
