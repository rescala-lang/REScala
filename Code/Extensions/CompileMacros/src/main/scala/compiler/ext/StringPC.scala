package compiler.ext

import clangast.expr.CExpr
import clangast.stmt.CStmt
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait StringPC extends PartialCompiler {
  def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = PartialFunction.empty
  
  def compileToString(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = PartialFunction.empty
  
  def hasInjectiveToString(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compilePrint(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CStmt] = PartialCompiler.ensurePC[StringPC](p, _.compilePrint)

  def compileToString(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CExpr] = PartialCompiler.ensurePC[StringPC](p, _.compileToString)

  def hasInjectiveToString(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean ] = PartialCompiler.ensurePC[StringPC](p, _.hasInjectiveToString)
}
