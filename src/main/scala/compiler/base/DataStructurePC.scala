package compiler.base

import clangast.decl.CFunctionDecl
import clangast.expr.{CCallExpr, CExpr}
import clangast.stmt.CStmt
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait DataStructurePC extends PartialCompiler {
  def usesRefCount(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = PartialFunction.empty

  def compileRetain(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialFunction.empty

  def compileRelease(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def usesRefCount(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = PartialCompiler.ensurePC[DataStructurePC](p, _.usesRefCount)

  def compileRetain(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialCompiler.ensurePC[DataStructurePC](p, _.compileRetain)

  def compileRelease(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialCompiler.ensurePC[DataStructurePC](p, _.compileRelease)
}
