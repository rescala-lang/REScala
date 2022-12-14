package compiler.ext

import api.CHelperFun
import clangast.expr.{CCallExpr, CDeclRefExpr, CExpr}
import compiler.context.TranslationContext
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.base.*

import scala.quoted.*

object HelperFunFragment extends ApplyIFFragment {
  override def compileApply(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Apply, CExpr] = {
    import quotes.reflect.*

    {
      case Apply(Select(helper @ Ident(name), "apply"), args) if helper.tpe <:< TypeRepr.of[CHelperFun] =>
        CCallExpr(
          CDeclRefExpr(name),
          args.map(dispatch[TermIFFragment](_.compileTermToCExpr))
        )
    }
  }
}
