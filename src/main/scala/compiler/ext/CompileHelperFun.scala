package compiler.ext

import api.CHelperFun
import clangast.expr.{CCallExpr, CExpr, CUnlinkedCallExpr}
import compiler.context.TranslationContext
import compiler.CompilerCascade
import compiler.base.*

import scala.quoted.*

object CompileHelperFun extends ApplyPC {
  override def compileApply(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*

      {
        case Apply(Select(helper@Ident(name), "apply"), args) if helper.tpe <:< TypeRepr.of[CHelperFun] =>
          CUnlinkedCallExpr(
            name,
            args.map(cascade.dispatch(_.compileTermToCExpr))
          )
      }
    }
}
