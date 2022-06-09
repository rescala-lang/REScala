package compiler.base

import clangast.CASTNode
import clangast.expr.binaryop.CAssignmentExpr
import clangast.expr.*
import clangast.stmt.*
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait TermPC extends PartialCompiler {
  def compileTerm(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CASTNode] = PartialFunction.empty

  def compileTermToCStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CStmt] = PartialFunction.empty

  def compileTermToCExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CExpr] = PartialFunction.empty

  def compileLiteral(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Literal, CExpr] = PartialFunction.empty

  def compileAssign(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Assign, CAssignmentExpr] = PartialFunction.empty

  def compileBlockToCStmtExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Block, CStmtExpr] = PartialFunction.empty

  def compileBlockToCCompoundStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Block, CCompoundStmt] = PartialFunction.empty

  def compileBlockToFunctionBody(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Block, CCompoundStmt] = PartialFunction.empty

  def compileIfToCIfStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.If, CIfStmt] = PartialFunction.empty

  def compileIfToCConditionalOperator(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.If, CConditionalOperator] = PartialFunction.empty

  def compileReturn(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Return, CReturnStmt] = PartialFunction.empty

  def compileWhile(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.While, CWhileStmt] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileTerm(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CASTNode] = PartialCompiler.ensurePC[TermPC](p, _.compileTerm)

  def compileTermToCStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CStmt] = PartialCompiler.ensurePC[TermPC](p, _.compileTermToCStmt)

  def compileTermToCExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CExpr] = PartialCompiler.ensurePC[TermPC](p, _.compileTermToCExpr)

  def compileLiteral(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Literal, CExpr] = PartialCompiler.ensurePC[TermPC](p, _.compileLiteral)

  def compileAssign(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Assign, CAssignmentExpr] = PartialCompiler.ensurePC[TermPC](p, _.compileAssign)

  def compileBlockToCStmtExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Block, CStmtExpr] = PartialCompiler.ensurePC[TermPC](p, _.compileBlockToCStmtExpr)

  def compileBlockToCCompoundStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Block, CCompoundStmt] = PartialCompiler.ensurePC[TermPC](p, _.compileBlockToCCompoundStmt)

  def compileBlockToFunctionBody(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Block, CCompoundStmt] = PartialCompiler.ensurePC[TermPC](p, _.compileBlockToFunctionBody)

  def compileIfToCIfStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.If, CIfStmt] = PartialCompiler.ensurePC[TermPC](p, _.compileIfToCIfStmt)

  def compileIfToCConditionalOperator(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.If, CConditionalOperator] = PartialCompiler.ensurePC[TermPC](p, _.compileIfToCConditionalOperator)

  def compileReturn(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Return, CReturnStmt] = PartialCompiler.ensurePC[TermPC](p, _.compileReturn)

  def compileWhile(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.While, CWhileStmt] = PartialCompiler.ensurePC[TermPC](p, _.compileWhile)
}
