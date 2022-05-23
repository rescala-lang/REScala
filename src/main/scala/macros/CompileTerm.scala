package macros

import clangast.CASTNode
import clangast.expr.*
import clangast.expr.binaryop.CAssignmentExpr
import clangast.stmt.*
import macros.ScalaToC.*
import macros.CompileDefinition.*
import macros.CompileRef.*
import macros.CompileApply.*
import macros.CompileStatement.*
import macros.CompileMatch.*

import scala.annotation.tailrec
import scala.quoted.*

object CompileTerm {
  @tailrec
  def compileTerm(using Quotes)(term: quotes.reflect.Term, ctx: TranslationContext): CASTNode = {
    import quotes.reflect.*

    term match {
      case ref: Ref => compileRef(ref, ctx)
      case Literal(UnitConstant()) => CNullStmt
      case literal: Literal => compileLiteral(literal, ctx)
      case apply: Apply => compileApply(apply, ctx)
      case assign: Assign => compileAssign(assign, ctx)
      case Block(List(defDef: DefDef), _: Closure) => compileDefDef(defDef, ctx)
      case Block(List(defDef: DefDef), Literal(UnitConstant())) => compileDefDef(defDef, ctx)
      case block: Block => compileBlockToCCompoundStmt(block, ctx)
      case ifTerm: If => compileIfToCIfStmt(ifTerm, ctx)
      case matchTerm: Match => compileMatchToCIfStmt(matchTerm, ctx)
      case ret: Return => compileReturn(ret, ctx)
      case inlined: Inlined => compileTerm(inlined.underlyingArgument, ctx)
      case whileTerm: While => compileWhile(whileTerm, ctx)
      case typed: Typed => compileTerm(typed.expr, ctx)
      case _ => throw new MatchError(term.show(using Printer.TreeStructure))
    }
  }

  def compileTermToCStmt(using Quotes)(term: quotes.reflect.Term, ctx: TranslationContext): CStmt = {
    import quotes.reflect.*

    compileTerm(term, ctx) match {
      case stmt: CStmt => stmt
      case expr: CExpr => CExprStmt(expr)
      case _ => throw new MatchError(term.show(using Printer.TreeStructure))
    }
  }

  def compileTermToCExpr(using Quotes)(term: quotes.reflect.Term, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    term match {
      case block: Block => compileBlockToCStmtExpr(block, ctx)
      case ifTerm: If => compileIfToCConditionalOperator(ifTerm, ctx)
      case matchTerm: Match => compileMatchToCStmtExpr(matchTerm, ctx)
      case _ =>
        compileTerm(term, ctx) match {
          case expr: CExpr => expr
          case _ => throw new MatchError(term.show(using Printer.TreeStructure))
        }
    }
  }

  def compileLiteral(using Quotes)(literal: quotes.reflect.Literal, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    literal match {
      case Literal(BooleanConstant(false)) => CFalseLiteral
      case Literal(BooleanConstant(true)) => CTrueLiteral
      case Literal(ByteConstant(x)) => CIntegerLiteral(x)
      case Literal(ShortConstant(x)) => CIntegerLiteral(x)
      case Literal(IntConstant(x)) => CIntegerLiteral(x)
      case Literal(LongConstant(x)) => CLongLiteral(x)
      case Literal(FloatConstant(x)) => CFloatLiteral(x)
      case Literal(DoubleConstant(x)) => CDoubleLiteral(x)
      case Literal(CharConstant(x)) => CCharacterLiteral(x)
      case Literal(StringConstant(x)) => CStringLiteral(x)
      case Literal(NullConstant()) => CNullLiteral
      case _ => throw new MatchError(literal.show(using Printer.TreeStructure))
    }
  }

  def compileAssign(using Quotes)(assign: quotes.reflect.Assign, ctx: TranslationContext): CAssignmentExpr = {
    import quotes.reflect.*

    val Assign(lhs, rhs) = assign

    // when is it a dereference on lhs necessary?
    CAssignmentExpr(compileTermToCExpr(lhs, ctx), compileTermToCExpr(rhs, ctx))
  }

  def compileBlockToCStmtExpr(using Quotes)(block: quotes.reflect.Block, ctx: TranslationContext): CStmtExpr = {
    import quotes.reflect.*

    val cCompoundStmt = compileBlockToCCompoundStmt(block, ctx)

    CStmtExpr(cCompoundStmt)
  }

  def compileBlockToCCompoundStmt(using Quotes)(block: quotes.reflect.Block, ctx: TranslationContext): CCompoundStmt = {
    import quotes.reflect.*

    val compiledStatements = block.statements.map(compileStatementToCStmt(_, ctx))

    val stmtList = block.expr.match {
      case Literal(UnitConstant()) => compiledStatements
      case _ => compiledStatements.appended(compileTermToCStmt(block.expr, ctx))
    }

    CCompoundStmt(stmtList)
  }

  def compileBlockToFunctionBody(using Quotes)(block: quotes.reflect.Block, ctx: TranslationContext): CCompoundStmt = {
    import quotes.reflect.*

    val Block(statements, expr) = block

    val compiledStatements = statements.map(compileStatementToCStmt(_, ctx))

    val stmtList = expr match {
      case Literal(UnitConstant()) => compiledStatements
      case _ if expr.tpe =:= TypeRepr.of[Unit] =>
        compiledStatements.appended(compileTermToCStmt(expr, ctx))
      case ret: Return => compiledStatements.appended(compileReturn(ret, ctx))
      case _ => compiledStatements.appended(CReturnStmt(Some(compileTermToCExpr(expr, ctx))))
    }

    CCompoundStmt(stmtList)
  }

  def compileIfToCIfStmt(using Quotes)(ifTerm: quotes.reflect.If, ctx: TranslationContext): CIfStmt = {
    import quotes.reflect.*

    val If(cond, thenp, elsep) = ifTerm

    val condExpr = compileTermToCExpr(cond, ctx)
    val thenpStmt = compileTermToCStmt(thenp, ctx)
    val elsepStmt = elsep match {
      case Literal(UnitConstant()) => None
      case _ => Some(compileTermToCStmt(elsep, ctx))
    }

    CIfStmt(condExpr, thenpStmt, elsepStmt)
  }

  def compileIfToCConditionalOperator(using Quotes)(ifTerm: quotes.reflect.If, ctx: TranslationContext): CConditionalOperator = {
    import quotes.reflect.*

    val If(cond, thenp, elsep) = ifTerm

    val condExpr = compileTermToCExpr(cond, ctx)
    val thenpExpr = compileTermToCExpr(thenp, ctx)
    val elsepExpr = compileTermToCExpr(elsep, ctx)

    CConditionalOperator(condExpr, thenpExpr, elsepExpr)
  }

  def compileReturn(using Quotes)(ret: quotes.reflect.Return, ctx: TranslationContext): CReturnStmt = {
    import quotes.reflect.*

    ret.expr match {
      case Literal(UnitConstant) => CReturnStmt()
      case term => CReturnStmt(Some(compileTermToCExpr(term, ctx)))
    }
  }

  def compileWhile(using Quotes)(whileTerm: quotes.reflect.While, ctx: TranslationContext): CWhileStmt = {
    import quotes.reflect.*

    val While(cond, body) = whileTerm

    val compiledCond = compileTermToCExpr(cond, ctx)

    val compiledBody = body match {
      case block: Block => compileBlockToCCompoundStmt(block, ctx)
      case term => CCompoundStmt(List(compileTermToCStmt(term, ctx)))
    }

    CWhileStmt(compiledCond, compiledBody)
  }
}
