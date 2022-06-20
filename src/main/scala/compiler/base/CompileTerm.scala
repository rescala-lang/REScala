package compiler.base

import clangast.given
import clangast.CASTNode
import clangast.decl.CVarDecl
import clangast.expr.binaryop.CAssignmentExpr
import clangast.expr.*
import clangast.stmt.*
import compiler.context.TranslationContext
import compiler.CompilerCascade
import compiler.base.CompileDataStructure.{retain, release}

import scala.annotation.tailrec
import scala.quoted.*

object CompileTerm extends TermPC {
  override def compileTerm(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Term, CASTNode] = {
      import quotes.reflect.*

      {
        case ref: Ref => cascade.dispatch(_.compileRef)(ref)
        case Literal(UnitConstant()) => CNullStmt
        case literal: Literal => cascade.dispatch(_.compileLiteral)(literal)
        case apply: Apply => cascade.dispatch(_.compileApply)(apply)
        case assign: Assign => cascade.dispatch(_.compileAssign)(assign)
        case Block(List(defDef: DefDef), _: Closure) => cascade.dispatch(_.compileDefDef)(defDef)
        case Block(List(defDef: DefDef), Literal(UnitConstant())) => cascade.dispatch(_.compileDefDef)(defDef)
        case block: Block => cascade.dispatch(_.compileBlockToCCompoundStmt)(block)
        case ifTerm: If => cascade.dispatch(_.compileIfToCIfStmt)(ifTerm)
        case matchTerm: Match => cascade.dispatch(_.compileMatchToCStmt)(matchTerm)
        case ret: Return => cascade.dispatch(_.compileReturn)(ret)
        case inlined: Inlined => cascade.dispatch(_.compileTerm)(inlined.underlyingArgument)
        case whileTerm: While => cascade.dispatch(_.compileWhile)(whileTerm)
        case typed: Typed => cascade.dispatch(_.compileTerm)(typed.expr)
      }
    }

  override def compileTermToCStmt(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Term, CStmt] = {
      import quotes.reflect.*

      PartialFunction.fromFunction(cascade.dispatch(_.compileTerm)).andThen {
        case stmt: CStmt => stmt
        case CStmtExpr(cCompoundStmt) => cCompoundStmt
        case expr: CExpr => CExprStmt(expr)
      }
    }

  override def compileTermToCExpr(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Term, CExpr] = ((term: quotes.reflect.Term) => {
      import quotes.reflect.*

      term match {
        case block: Block => Some(cascade.dispatch(_.compileBlockToCStmtExpr)(block))
        case ifTerm: If => Some(cascade.dispatch(_.compileIfToCConditionalOperator)(ifTerm))
        case matchTerm: Match => Some(cascade.dispatch(_.compileMatchToCExpr)(matchTerm))
        case _ =>
          cascade.dispatch(_.compileTerm)(term) match {
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
    PartialFunction[quotes.reflect.Assign, CExpr] = {
      import quotes.reflect.*

      {
        case Assign(lhs, rhs) =>
          val lhsCompiled = cascade.dispatch(_.compileTermToCExpr)(lhs)
          val rhsCompiled = cascade.dispatch(_.compileTermToCExpr)(rhs)

          if cascade.dispatch(_.usesRefCount)(lhs.tpe) then
            val tempDecl = CVarDecl("temp", cascade.dispatch(_.compileTypeRepr)(rhs.tpe), Some(lhsCompiled))

            CStmtExpr(CCompoundStmt(List(
              tempDecl,
              CAssignmentExpr(
                lhsCompiled,
                retain(rhsCompiled, rhs.tpe)
              ),
              release(tempDecl.ref, lhs.tpe, CFalseLiteral).get
            )))
          else
            CAssignmentExpr(lhsCompiled, rhsCompiled)
      }
    }

  override def compileBlockToCStmtExpr(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Block, CStmtExpr] =
      PartialFunction.fromFunction(cascade.dispatch(_.compileBlockToCCompoundStmt).andThen(CStmtExpr.apply))

  override def compileBlockToCCompoundStmt(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Block, CCompoundStmt] = {
      import quotes.reflect.*

      {
        case Block(statements, expr) =>
          val compiledStatements = statements.map(cascade.dispatch(_.compileStatementToCStmt))

          val stmtList = expr.match {
            case Literal(UnitConstant()) => compiledStatements
            case _ => compiledStatements.appended(cascade.dispatch(_.compileTermToCStmt)(expr))
          }

          CCompoundStmt(stmtList)
      }
    }

  override def compileBlockToFunctionBody(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Block, CCompoundStmt] = {
      import quotes.reflect.*

      {
        case Block(statements, expr) =>
          val compiledStatements = statements.map(cascade.dispatch(_.compileStatementToCStmt))

          val stmtList = expr match {
            case Literal(UnitConstant()) => compiledStatements
            case _ if expr.tpe =:= TypeRepr.of[Unit] =>
              compiledStatements.appended(cascade.dispatch(_.compileTermToCStmt)(expr))
            case ret: Return => compiledStatements.appended(cascade.dispatch(_.compileReturn)(ret))
            case _ => compiledStatements.appended(CReturnStmt(Some(cascade.dispatch(_.compileTermToCExpr)(expr))))
          }

          CCompoundStmt(stmtList)
      }
    }

  override def compileIfToCIfStmt(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.If, CIfStmt] = {
      import quotes.reflect.*

      {
        case If(cond, thenp, elsep) =>
          val condExpr = cascade.dispatch(_.compileTermToCExpr)(cond)
          val thenpStmt = cascade.dispatch(_.compileTermToCStmt)(thenp)
          val elsepStmt = elsep match {
            case Literal(UnitConstant()) => None
            case _ => Some(cascade.dispatch(_.compileTermToCStmt)(elsep))
          }

          CIfStmt(condExpr, thenpStmt, elsepStmt)
      }
    }

  override def compileIfToCConditionalOperator(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.If, CConditionalOperator] = {
      import quotes.reflect.*

      {
        case If(cond, thenp, elsep) =>
          val condExpr = cascade.dispatch(_.compileTermToCExpr)(cond)
          val thenpExpr = cascade.dispatch(_.compileTermToCExpr)(thenp)
          val elsepExpr = cascade.dispatch(_.compileTermToCExpr)(elsep)

          CConditionalOperator(condExpr, thenpExpr, elsepExpr)
      }
    }

  override def compileReturn(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Return, CReturnStmt] = {
      import quotes.reflect.*

      {
        case Return(Literal(UnitConstant), _) => CReturnStmt()
        case Return(term, _) => CReturnStmt(Some(cascade.dispatch(_.compileTermToCExpr)(term)))
      }
    }

  override def compileWhile(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.While, CWhileStmt] = {
      import quotes.reflect.*
    
      {
        case While(cond, body ) =>
          val compiledCond = cascade.dispatch(_.compileTermToCExpr)(cond)

          val compiledBody = body match {
            case block: Block => cascade.dispatch(_.compileBlockToCCompoundStmt)(block)
            case term => CCompoundStmt(List(cascade.dispatch(_.compileTermToCStmt)(term)))
          }

          CWhileStmt(compiledCond, compiledBody)
      }
    }
}
