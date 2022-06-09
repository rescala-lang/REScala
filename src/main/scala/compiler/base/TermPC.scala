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
    PartialFunction[quotes.reflect.Term, CASTNode] = p match {
      case termPC: TermPC => termPC.compileTerm
      case _ => PartialFunction.empty
    }

  def compileTermToCStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CStmt] = p match {
      case termPC: TermPC => termPC.compileTermToCStmt
      case _ => PartialFunction.empty
    }

  def compileTermToCExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CExpr] = p match {
      case termPC: TermPC => termPC.compileTermToCExpr
      case _ => PartialFunction.empty
    }

  def compileLiteral(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Literal, CExpr] = p match {
      case termPC: TermPC => termPC.compileLiteral
      case _ => PartialFunction.empty
    }

  def compileAssign(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Assign, CAssignmentExpr] = p match {
      case termPC: TermPC => termPC.compileAssign
      case _ => PartialFunction.empty
    }

  def compileBlockToCStmtExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Block, CStmtExpr] = p match {
      case termPC: TermPC => termPC.compileBlockToCStmtExpr
      case _ => PartialFunction.empty
    }

  def compileBlockToCCompoundStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Block, CCompoundStmt] = p match {
      case termPC: TermPC => termPC.compileBlockToCCompoundStmt
      case _ => PartialFunction.empty
    }

  def compileBlockToFunctionBody(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Block, CCompoundStmt] = p match {
      case termPC: TermPC => termPC.compileBlockToFunctionBody
      case _ => PartialFunction.empty
    }

  def compileIfToCIfStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.If, CIfStmt] = p match {
      case termPC: TermPC => termPC.compileIfToCIfStmt
      case _ => PartialFunction.empty
    }

  def compileIfToCConditionalOperator(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.If, CConditionalOperator] = p match {
      case termPC: TermPC => termPC.compileIfToCConditionalOperator
      case _ => PartialFunction.empty
    }

  def compileReturn(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Return, CReturnStmt] = p match {
      case termPC: TermPC => termPC.compileReturn
      case _ => PartialFunction.empty
    }

  def compileWhile(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.While, CWhileStmt] = p match {
      case termPC: TermPC => termPC.compileWhile
      case _ => PartialFunction.empty
    }
}
