package compiler.base

import clangast.expr.{CDeclRefExpr, CExpr}
import compiler.context.{TranslationContext, ValueDeclTC}
import compiler.CompilerCascade

import scala.quoted.*

object CompileRef extends RefPC {
  override def compileRef(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Ref, CExpr] = {
      import quotes.reflect.*

      {
        case ident: Ident => cascade.dispatch(_.compileIdent)(ident)
        case select: Select => cascade.dispatch(_.compileSelect)(select)
      }
    }

  private def compileIdentImpl(using Quotes)(using ctx: ValueDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Ident, CExpr] = ((ident: quotes.reflect.Ident) => {
      import quotes.reflect.*

      ctx.nameToDecl.get(ident.name).map(_.ref)
    }).unlift

  override def compileIdent(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Ident, CExpr] = ensureCtx[ValueDeclTC](compileIdentImpl)
}
