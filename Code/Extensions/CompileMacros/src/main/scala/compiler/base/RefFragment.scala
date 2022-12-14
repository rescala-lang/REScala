package compiler.base
import clangast.expr.CExpr
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.context.{TranslationContext, ValueDeclTC}

import scala.quoted.*

object RefFragment extends RefIFFragment {
  override def compileRef(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Ref, CExpr] = {
    import quotes.reflect.*

    {
      case ident: Ident   => dispatch[RefIFFragment](_.compileIdent)(ident)
      case select: Select => dispatch[SelectIFFragment](_.compileSelect)(select)
    }
  }

  override def compileIdent(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Ident, CExpr] = ensureCtx[ValueDeclTC] { ctx ?=>
    ((ident: quotes.reflect.Ident) => ctx.nameToDecl.get(ident.symbol.name).map(_.ref)).unlift
  }
}
