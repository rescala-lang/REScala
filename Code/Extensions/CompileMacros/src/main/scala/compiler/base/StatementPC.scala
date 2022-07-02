package compiler.base

import clangast.CASTNode
import clangast.stmt.CStmt
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait StatementPC extends PartialCompiler {
  def compileStatement(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Statement, CASTNode] = PartialFunction.empty

  def compileStatementToCStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Statement, CStmt] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileStatement(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Statement, CASTNode] = PartialCompiler.ensurePC[StatementPC](p, _.compileStatement)

  def compileStatementToCStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Statement, CStmt] = PartialCompiler.ensurePC[StatementPC](p, _.compileStatementToCStmt)
}
