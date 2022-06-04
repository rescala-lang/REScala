package compiler

import clangast.CASTNode
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.CAssignmentExpr
import clangast.stmt.*
import clangast.types.CType

import scala.annotation.targetName
import scala.quoted.*

class CompilerCascade(val partialCompilers: List[PartialCompiler]) {
  given CompilerCascade = this

  @targetName("flowTo")
  inline def ~~>(other: CompilerCascade): CompilerCascade = new CompilerCascade(this.partialCompilers ++ other.partialCompilers)
  
  @targetName("prepend")
  inline def ~>:(pc: PartialCompiler): CompilerCascade = new CompilerCascade(pc :: this.partialCompilers)

  private def reducePartialsForTree[R](using Quotes): [A <: quotes.reflect.Tree] => (PartialCompiler => PartialFunction[A, R]) => A => R =
    [A <: quotes.reflect.Tree] => (m: PartialCompiler => PartialFunction[A, R]) =>
      reducePartials(m, t => throw new MatchError(t.show(using quotes.reflect.Printer.TreeStructure)))

  private def reducePartialsForTypeRepr[R](using Quotes): [A <: quotes.reflect.TypeRepr] => (PartialCompiler => PartialFunction[A, R]) => A => R =
    [A <: quotes.reflect.TypeRepr] => (m: PartialCompiler => PartialFunction[A, R]) =>
      reducePartials(m, t => throw new MatchError(t.show(using quotes.reflect.Printer.TypeReprStructure)))

  private def reducePartials[A, R](m: PartialCompiler => PartialFunction[A, R], default: PartialFunction[A, R]): A => R =
    partialCompilers.map(m).foldRight(default) { (p, acc) => p.orElse(acc) }

  def compileTree(using Quotes, TranslationContext): quotes.reflect.Tree => CASTNode =
    reducePartialsForTree(_.compileTree)

  def compileStatement(using Quotes, TranslationContext): quotes.reflect.Statement => CASTNode =
    reducePartialsForTree(_.compileStatement)

  def compileStatementToCStmt(using Quotes, TranslationContext): quotes.reflect.Statement => CStmt =
    reducePartialsForTree(_.compileStatementToCStmt)

  def compileDefinition(using Quotes, TranslationContext): quotes.reflect.Definition => CDecl =
    reducePartialsForTree(_.compileDefinition)

  def compileDefDef(using Quotes, TranslationContext): quotes.reflect.DefDef => CFunctionDecl =
    reducePartialsForTree(_.compileDefDef)

  def compileValDefToCVarDecl(using Quotes, TranslationContext): quotes.reflect.ValDef => CVarDecl =
    reducePartialsForTree(_.compileValDefToCVarDecl)

  def compileValDefToCParmVarDecl(using Quotes, TranslationContext): quotes.reflect.ValDef => CParmVarDecl =
    reducePartialsForTree(_.compileValDefToCParmVarDecl)

  def compileTerm(using Quotes, TranslationContext): quotes.reflect.Term => CASTNode =
    reducePartialsForTree(_.compileTerm)

  def compileTermToCStmt(using Quotes, TranslationContext): quotes.reflect.Term => CStmt =
    reducePartialsForTree(_.compileTermToCStmt)

  def compileTermToCExpr(using Quotes, TranslationContext): quotes.reflect.Term => CExpr =
    reducePartialsForTree(_.compileTermToCExpr)

  def compileRef(using Quotes, TranslationContext): quotes.reflect.Ref => CExpr =
    reducePartialsForTree(_.compileRef)

  def compileIdent(using Quotes, TranslationContext): quotes.reflect.Ident => CExpr =
    reducePartialsForTree(_.compileIdent)

  def compileSelect(using Quotes, TranslationContext): quotes.reflect.Select => CExpr =
    reducePartialsForTree(_.compileSelect)

  def compileLiteral(using Quotes, TranslationContext): quotes.reflect.Literal => CExpr =
    reducePartialsForTree(_.compileLiteral)

  def compileApply(using Quotes, TranslationContext): quotes.reflect.Apply => CExpr =
    reducePartialsForTree(_.compileApply)

  def compileAssign(using Quotes, TranslationContext): quotes.reflect.Assign => CAssignmentExpr =
    reducePartialsForTree(_.compileAssign)

  def compileBlockToCStmtExpr(using Quotes, TranslationContext): quotes.reflect.Block => CStmtExpr =
    reducePartialsForTree(_.compileBlockToCStmtExpr)

  def compileBlockToCCompoundStmt(using Quotes, TranslationContext): quotes.reflect.Block => CCompoundStmt =
    reducePartialsForTree(_.compileBlockToCCompoundStmt)

  def compileBlockToFunctionBody(using Quotes, TranslationContext): quotes.reflect.Block => CCompoundStmt =
    reducePartialsForTree(_.compileBlockToFunctionBody)

  def compileIfToCIfStmt(using Quotes, TranslationContext): quotes.reflect.If => CIfStmt =
    reducePartialsForTree(_.compileIfToCIfStmt)

  def compileIfToCConditionalOperator(using Quotes, TranslationContext): quotes.reflect.If => CConditionalOperator =
    reducePartialsForTree(_.compileIfToCConditionalOperator)

  def compileMatchToCIfStmt(using Quotes, TranslationContext): quotes.reflect.Match => CIfStmt =
    reducePartialsForTree(_.compileMatchToCIfStmt)

  def compileMatchToCStmtExpr(using Quotes, TranslationContext): quotes.reflect.Match => CStmtExpr =
    reducePartialsForTree(_.compileMatchToCStmtExpr)

  def compileReturn(using Quotes, TranslationContext): quotes.reflect.Return => CReturnStmt =
    reducePartialsForTree(_.compileReturn)

  def compileWhile(using Quotes, TranslationContext): quotes.reflect.While => CWhileStmt =
    reducePartialsForTree(_.compileWhile)

  def compileCaseDef(using Quotes, TranslationContext): ((quotes.reflect.CaseDef, quotes.reflect.Term)) => (Option[CExpr], List[CVarDecl], List[CStmt]) =
    reducePartials(_.compileCaseDef, { case (caseDef, _) => throw new MatchError(caseDef.show(using quotes.reflect.Printer.TreeStructure)) })

  def compilePattern(using Quotes, TranslationContext): ((quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr)) => (Option[CExpr], List[CVarDecl]) =
    reducePartials(_.compilePattern, { case (pattern, _, _) => throw new MatchError(pattern.show(using quotes.reflect.Printer.TreeStructure)) })

  def compileTypeRepr(using Quotes, TranslationContext): quotes.reflect.TypeRepr => CType =
    reducePartialsForTypeRepr(_.compileTypeRepr)
}

object CompilerCascade {
  def apply(partialCompilers: PartialCompiler*): CompilerCascade = new CompilerCascade(partialCompilers.toList)
}