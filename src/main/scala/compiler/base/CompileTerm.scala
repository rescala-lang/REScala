package compiler.base

import clangast.CASTNode
import clangast.expr.binaryop.CAssignmentExpr
import clangast.expr.*
import clangast.stmt.*
import compiler.{CompilerCascade, PartialCompiler, TranslationContext}

import scala.annotation.tailrec
import scala.quoted.*

object CompileTerm extends PartialCompiler {
  override def compileTerm(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Term, CASTNode] = {
      import quotes.reflect.*

      {
        case ref: Ref => cascade.compileRef(ref)
        case Literal(UnitConstant()) => CNullStmt
        case literal: Literal => cascade.compileLiteral(literal)
        case apply: Apply => cascade.compileApply(apply)
        case assign: Assign => cascade.compileAssign(assign)
        case Block(List(defDef: DefDef), _: Closure) => cascade.compileDefDef(defDef)
        case Block(List(defDef: DefDef), Literal(UnitConstant())) => cascade.compileDefDef(defDef)
        case block: Block => cascade.compileBlockToCCompoundStmt(block)
        case ifTerm: If => cascade.compileIfToCIfStmt(ifTerm)
        case matchTerm: Match => cascade.compileMatchToCIfStmt(matchTerm)
        case ret: Return => cascade.compileReturn(ret)
        case inlined: Inlined => cascade.compileTerm(inlined.underlyingArgument)
        case whileTerm: While => cascade.compileWhile(whileTerm)
        case typed: Typed => cascade.compileTerm(typed.expr)
      }
    }

  override def compileTermToCStmt(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Term, CStmt] = {
      import quotes.reflect.*

      PartialFunction.fromFunction(cascade.compileTerm).andThen {
        case stmt: CStmt => stmt
        case expr: CExpr => CExprStmt(expr)
      }
    }

  override def compileTermToCExpr(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Term, CExpr] = ((term: quotes.reflect.Term) => {
      import quotes.reflect.*

      term match {
        case block: Block => Some(cascade.compileBlockToCStmtExpr(block))
        case ifTerm: If => Some(cascade.compileIfToCConditionalOperator(ifTerm))
        case matchTerm: Match => Some(cascade.compileMatchToCStmtExpr(matchTerm))
        case _ =>
          cascade.compileTerm(term) match {
            case expr: CExpr => Some(expr)
            case _ => None
          }
      }
    }).unlift

  override def compileLiteral(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Literal, CExpr] = {
      import quotes.reflect.*

      {
        case Literal(BooleanConstant(false)) => CFalseLiteral
        case Literal(BooleanConstant(true)) => CTrueLiteral
        case Literal(ByteConstant(x)) => CIntegerLiteral(x)
        case Literal(ShortConstant(x)) => CIntegerLiteral(x)
        case Literal(IntConstant(x)) => CIntegerLiteral(x)
        case Literal(LongConstant(x)) => CLongLiteral(x)
        case Literal(FloatConstant(x)) => CFloatLiteral(x)
        case Literal(DoubleConstant(x)) => CDoubleLiteral(x)
        case Literal(CharConstant(x)) => CCharacterLiteral(x)
        case Literal(NullConstant()) => CNullLiteral
      }
    }

  override def compileAssign(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Assign, CAssignmentExpr] = {
      import quotes.reflect.*

      {
        case Assign(lhs, rhs) =>
          CAssignmentExpr(cascade.compileTermToCExpr(lhs), cascade.compileTermToCExpr(rhs))
      }
    }

  override def compileBlockToCStmtExpr(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Block, CStmtExpr] =
      PartialFunction.fromFunction(cascade.compileBlockToCCompoundStmt.andThen(CStmtExpr.apply))

  override def compileBlockToCCompoundStmt(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Block, CCompoundStmt] = {
      import quotes.reflect.*

      {
        case Block(statements, expr) =>
          val compiledStatements = statements.map(cascade.compileStatementToCStmt)

          val stmtList = expr.match {
            case Literal(UnitConstant()) => compiledStatements
            case _ => compiledStatements.appended(cascade.compileTermToCStmt(expr))
          }

          CCompoundStmt(stmtList)
      }
    }

  override def compileBlockToFunctionBody(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Block, CCompoundStmt] = {
      import quotes.reflect.*

      {
        case Block(statements, expr) =>
          val compiledStatements = statements.map(cascade.compileStatementToCStmt)

          val stmtList = expr match {
            case Literal(UnitConstant()) => compiledStatements
            case _ if expr.tpe =:= TypeRepr.of[Unit] =>
              compiledStatements.appended(cascade.compileTermToCStmt(expr))
            case ret: Return => compiledStatements.appended(cascade.compileReturn(ret))
            case _ => compiledStatements.appended(CReturnStmt(Some(cascade.compileTermToCExpr(expr))))
          }

          CCompoundStmt(stmtList)
      }
    }

  override def compileIfToCIfStmt(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.If, CIfStmt] = {
      import quotes.reflect.*

      {
        case If(cond, thenp, elsep) =>
          val condExpr = cascade.compileTermToCExpr(cond)
          val thenpStmt = cascade.compileTermToCStmt(thenp)
          val elsepStmt = elsep match {
            case Literal(UnitConstant()) => None
            case _ => Some(cascade.compileTermToCStmt(elsep))
          }

          CIfStmt(condExpr, thenpStmt, elsepStmt)
      }
    }

  override def compileIfToCConditionalOperator(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.If, CConditionalOperator] = {
      import quotes.reflect.*

      {
        case If(cond, thenp, elsep) =>
          val condExpr = cascade.compileTermToCExpr(cond)
          val thenpExpr = cascade.compileTermToCExpr(thenp)
          val elsepExpr = cascade.compileTermToCExpr(elsep)

          CConditionalOperator(condExpr, thenpExpr, elsepExpr)
      }
    }

  override def compileReturn(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Return, CReturnStmt] = {
      import quotes.reflect.*

      {
        case Return(Literal(UnitConstant), _) => CReturnStmt()
        case Return(term, _) => CReturnStmt(Some(cascade.compileTermToCExpr(term)))
      }
    }

  override def compileWhile(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.While, CWhileStmt] = {
      import quotes.reflect.*
    
      {
        case While(cond, body ) =>
          val compiledCond = cascade.compileTermToCExpr(cond)

          val compiledBody = body match {
            case block: Block => cascade.compileBlockToCCompoundStmt(block)
            case term => CCompoundStmt(List(cascade.compileTermToCStmt(term)))
          }

          CWhileStmt(compiledCond, compiledBody)
      }
    }
}
