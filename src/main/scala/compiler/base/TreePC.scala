package compiler.base

import clangast.CASTNode
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait TreePC extends PartialCompiler {
  def compileTree(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Tree, CASTNode] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileTree(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Tree, CASTNode] = PartialCompiler.ensurePC[TreePC](p, _.compileTree)
}
