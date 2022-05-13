package macros

import clangast.expr.{CDeclRefExpr, CExpr}
import macros.ScalaToC.*
import macros.CompileSelect.*

import scala.quoted.*

object CompileRef {
  def compileRef(using Quotes)(ref: quotes.reflect.Ref, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    ref match {
      case ident: Ident => compileIdent(ident, ctx)
      case select: Select => compileSelect(select, ctx)
      case _ => throw new MatchError(ref.show(using Printer.TreeStructure))
    }
  }

  def compileIdent(using Quotes)(ident: quotes.reflect.Ident, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    ctx.nameToDecl.get(ident.name) match
      // possibly pointer instead of direct reference
      case Some(decl) => CDeclRefExpr(decl)
      // (maybe?) if no decl exists in ctx, use an unchecked string-based reference instead
      // can't create a new variable declaration because I don't know how to follow an ident to its definition
      case None => throw new MatchError(ident.show(using Printer.TreeStructure))
  }
}
