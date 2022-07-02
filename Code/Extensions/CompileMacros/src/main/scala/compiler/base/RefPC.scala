package compiler.base

import clangast.expr.CExpr
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait RefPC extends PartialCompiler {
  def compileRef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Ref, CExpr] = PartialFunction.empty

  def compileIdent(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Ident, CExpr] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileRef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Ref, CExpr] = PartialCompiler.ensurePC[RefPC](p, _.compileRef)

  def compileIdent(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Ident, CExpr] = PartialCompiler.ensurePC[RefPC](p, _.compileIdent)
}
