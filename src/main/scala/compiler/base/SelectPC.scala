package compiler.base

import clangast.expr.CExpr
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait SelectPC extends PartialCompiler {
  def compileSelect(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileSelect(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = p match {
      case selectPC: SelectPC => selectPC.compileSelect
      case _ => PartialFunction.empty
    }
}
