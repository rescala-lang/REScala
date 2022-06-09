package compiler.base

import clangast.decl.CVarDecl
import clangast.expr.{CExpr, CStmtExpr}
import clangast.stmt.{CIfStmt, CStmt}
import compiler.context.TranslationContext
import compiler.{CompilerCascade, PartialCompiler}

import scala.quoted.*

trait MatchPC extends PartialCompiler {
  def compileMatchToCIfStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Match, CIfStmt] = PartialFunction.empty

  def compileMatchToCStmtExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Match, CStmtExpr] = PartialFunction.empty

  def compileCaseDef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.CaseDef, quotes.reflect.Term), (Option[CExpr], List[CVarDecl], List[CStmt])] = PartialFunction.empty

  def compilePattern(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileMatchToCIfStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Match, CIfStmt] = p match {
      case matchPC: MatchPC => matchPC.compileMatchToCIfStmt
      case _ => PartialFunction.empty
    }

  def compileMatchToCStmtExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Match, CStmtExpr] = p match {
      case matchPC: MatchPC => matchPC.compileMatchToCStmtExpr
      case _ => PartialFunction.empty
    }

  def compileCaseDef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.CaseDef, quotes.reflect.Term), (Option[CExpr], List[CVarDecl], List[CStmt])] = p match {
      case matchPC: MatchPC => matchPC.compileCaseDef
      case _ => PartialFunction.empty
    }

  def compilePattern(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] = p match {
      case matchPC: MatchPC => matchPC.compilePattern
      case _ => PartialFunction.empty
    }
}
