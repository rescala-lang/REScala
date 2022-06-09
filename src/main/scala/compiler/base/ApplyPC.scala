package compiler.base

import clangast.expr.CExpr
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait ApplyPC extends PartialCompiler {
  def compileApply(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileApply(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = PartialCompiler.ensurePC[ApplyPC](p, _.compileApply)
}
