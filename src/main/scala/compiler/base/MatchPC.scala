package compiler.base

import clangast.decl.CVarDecl
import clangast.expr.{CExpr, CStmtExpr}
import clangast.stmt.{CIfStmt, CStmt}
import compiler.context.TranslationContext
import compiler.{CompilerCascade, PartialCompiler}

import scala.quoted.*

trait MatchPC extends PartialCompiler {
  def compileMatchToCStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Match, CStmt] = PartialFunction.empty

  def compileMatchToCExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Match, CExpr] = PartialFunction.empty

  def compileCaseDef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.CaseDef, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl], List[CStmt])] = PartialFunction.empty

  def compilePattern(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileMatchToCStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Match, CStmt] = PartialCompiler.ensurePC[MatchPC](p, _.compileMatchToCStmt)

  def compileMatchToCExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Match, CExpr] = PartialCompiler.ensurePC[MatchPC](p, _.compileMatchToCExpr)

  def compileCaseDef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.CaseDef, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl], List[CStmt])] =
      PartialCompiler.ensurePC[MatchPC](p, _.compileCaseDef)

  def compilePattern(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] =
      PartialCompiler.ensurePC[MatchPC](p, _.compilePattern)
}
