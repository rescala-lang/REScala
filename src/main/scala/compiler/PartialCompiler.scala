package compiler

import clangast.CASTNode
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.CAssignmentExpr
import clangast.stmt.*
import clangast.types.CType

import scala.quoted.*

trait PartialCompiler {
  def compileTree(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Tree, CASTNode] = PartialFunction.empty

  def compileStatement(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Statement, CASTNode] = PartialFunction.empty

  def compileStatementToCStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Statement, CStmt] = PartialFunction.empty

  def compileDefinition(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Definition, CDecl] = PartialFunction.empty

  def compileDefDef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.DefDef, CFunctionDecl] = PartialFunction.empty

  def compileValDefToCVarDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = PartialFunction.empty

  def compileValDefToCParmVarDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CParmVarDecl] = PartialFunction.empty

  def compileTerm(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CASTNode] = PartialFunction.empty

  def compileTermToCStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CStmt] = PartialFunction.empty

  def compileTermToCExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CExpr] = PartialFunction.empty

  def compileRef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Ref, CExpr] = PartialFunction.empty

  def compileIdent(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Ident, CExpr] = PartialFunction.empty

  def compileSelect(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = PartialFunction.empty

  def compileLiteral(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Literal, CExpr] = PartialFunction.empty

  def compileApply(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = PartialFunction.empty

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

  def compileMatchToCIfStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Match, CIfStmt] = PartialFunction.empty

  def compileMatchToCStmtExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Match, CStmtExpr] = PartialFunction.empty

  def compileReturn(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Return, CReturnStmt] = PartialFunction.empty

  def compileWhile(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.While, CWhileStmt] = PartialFunction.empty

  def compileCaseDef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.CaseDef, quotes.reflect.Term), (Option[CExpr], List[CVarDecl], List[CStmt])] = PartialFunction.empty

  def compilePattern(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] = PartialFunction.empty

  def compileTypeRepr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CType] = PartialFunction.empty
}
