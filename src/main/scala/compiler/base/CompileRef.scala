package compiler.base

import clangast.expr.{CDeclRefExpr, CExpr}
import compiler.{CompilerCascade, PartialCompiler, TranslationContext}

import scala.quoted.*

object CompileRef extends PartialCompiler {
  override def compileRef(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Ref, CExpr] = {
      import quotes.reflect.*
    
      {
        case ident: Ident => cascade.compileIdent(ident)
        case select: Select => cascade.compileSelect(select)
      }
    }

  override def compileIdent(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Ident, CExpr] = ((ident: quotes.reflect.Ident) => {
      import quotes.reflect.*

      ctx.nameToDecl.get(ident.name).map(CDeclRefExpr.apply)
    }).unlift
}
