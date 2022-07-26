package compiler.ext

import api2.CompiledReactive
import clangast.decl.CFunctionDecl
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait ReactivePC extends PartialCompiler {
  def compileReactiveTopLevelStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Statement, Unit] = PartialFunction.empty

  def compileReactive(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CompiledReactive] = PartialFunction.empty

  def compileReactiveExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CFunctionDecl] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileReactiveTopLevelStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Statement, Unit] = PartialCompiler.ensurePC[ReactivePC](p, _.compileReactiveTopLevelStmt)

  def compileReactive(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CompiledReactive] = PartialCompiler.ensurePC[ReactivePC](p, _.compileReactive)

  def compileReactiveExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CFunctionDecl] = PartialCompiler.ensurePC[ReactivePC](p, _.compileReactiveExpr)
}
