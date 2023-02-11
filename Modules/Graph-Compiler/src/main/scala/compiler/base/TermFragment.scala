package compiler.base

import clangast.given
import clangast.CASTNode
import clangast.decl.CVarDecl
import clangast.expr.binaryop.CAssignmentExpr
import clangast.expr.*
import clangast.stmt.*
import compiler.context.TranslationContext
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.base.DataStructureFragment.{retain, release}

import scala.annotation.tailrec
import scala.quoted.*

object TermFragment extends TermIFFragment {
  override def compileTerm(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Term, CASTNode] = {
    import quotes.reflect.*

    {
      case ref: Ref                                => dispatch[RefIFFragment](_.compileRef)(ref)
      case Literal(UnitConstant())                 => CNullStmt
      case literal: Literal                        => dispatch[TermIFFragment](_.compileLiteral)(literal)
      case apply: Apply                            => dispatch[ApplyIFFragment](_.compileApply)(apply)
      case assign: Assign                          => dispatch[TermIFFragment](_.compileAssign)(assign)
      case Block(List(defDef: DefDef), _: Closure) => dispatch[DefinitionIFFragment](_.compileDefDef)(defDef)
      case Block(List(defDef: DefDef), Literal(UnitConstant())) =>
        dispatch[DefinitionIFFragment](_.compileDefDef)(defDef)
      case block: Block     => dispatch[TermIFFragment](_.compileBlockToCCompoundStmt)(block)
      case ifTerm: If       => dispatch[TermIFFragment](_.compileIfToCIfStmt)(ifTerm)
      case matchTerm: Match => dispatch[MatchIFFragment](_.compileMatchToCStmt)(matchTerm)
      case ret: Return      => dispatch[TermIFFragment](_.compileReturn)(ret)
      case inlined: Inlined => dispatch[TermIFFragment](_.compileTerm)(inlined.underlyingArgument)
      case whileTerm: While => dispatch[TermIFFragment](_.compileWhile)(whileTerm)
      case typed: Typed     => dispatch[TermIFFragment](_.compileTerm)(typed.expr)
    }
  }

  override def compileTermToCStmt(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Term, CStmt] = {
    import quotes.reflect.*

    PartialFunction.fromFunction(dispatch[TermIFFragment](_.compileTerm)).andThen {
      case stmt: CStmt              => stmt
      case CStmtExpr(cCompoundStmt) => cCompoundStmt
      case expr: CExpr              => CExprStmt(expr)
    }
  }

  override def compileTermToCExpr(using Quotes)(using fc: FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Term, CExpr] = ((term: quotes.reflect.Term) => {
    import quotes.reflect.*

    term match {
      case block: Block     => Some(dispatch[TermIFFragment](_.compileBlockToCStmtExpr)(block))
      case ifTerm: If       => Some(dispatch[TermIFFragment](_.compileIfToCConditionalOperator)(ifTerm))
      case matchTerm: Match => Some(dispatch[MatchIFFragment](_.compileMatchToCExpr)(matchTerm))
      case inlined: Inlined => fc.dispatchLifted[TermIFFragment](_.compileTermToCExpr)(inlined.underlyingArgument)
      case _ =>
        dispatch[TermIFFragment](_.compileTerm)(term) match {
          case expr: CExpr => Some(expr)
          case _           => None
        }
    }
  }).unlift

  override def compileLiteral(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Literal, CExpr] = {
    import quotes.reflect.*

    {
      case Literal(BooleanConstant(false)) => CFalseLiteral
      case Literal(BooleanConstant(true))  => CTrueLiteral
      case Literal(ByteConstant(x))        => CIntegerLiteral(x)
      case Literal(ShortConstant(x))       => CIntegerLiteral(x)
      case Literal(IntConstant(x))         => CIntegerLiteral(x)
      case Literal(LongConstant(x))        => CLongLiteral(x)
      case Literal(FloatConstant(x))       => CFloatLiteral(x)
      case Literal(DoubleConstant(x))      => CDoubleLiteral(x)
      case Literal(CharConstant(x))        => CCharacterLiteral(x)
      case Literal(NullConstant())         => CNullLiteral
    }
  }

  override def compileAssign(using Quotes)(using FragmentedCompiler)(using
      ctx: TranslationContext
  ): PartialFunction[quotes.reflect.Assign, CExpr] = {
    import quotes.reflect.*

    {
      case Assign(lhs, rhs) =>
        val lhsCompiled = dispatch[TermIFFragment](_.compileTermToCExpr)(lhs)
        val rhsCompiled = dispatch[TermIFFragment](_.compileTermToCExpr)(rhs)

        if dispatch[DataStructureIFFragment](_.usesRefCount)(lhs.tpe) then
          val tempName = ctx.uniqueValueName("_temp")
          val tempDecl = CVarDecl(tempName, dispatch[TypeIFFragment](_.compileTypeRepr)(rhs.tpe), Some(lhsCompiled))

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

  override def compileBlockToCStmtExpr(using Quotes)(using FragmentedCompiler)(using
      ctx: TranslationContext
  ): PartialFunction[quotes.reflect.Block, CStmtExpr] = {
    import quotes.reflect.*

    {
      case Block(statements, expr) =>
        val compiledStatements = statements.map(dispatch[StatementIFFragment](_.compileStatementToCStmt))

        val releaseLocalVars = DataStructureFragment.releaseLocalVars(compiledStatements)

        val stmtList: List[CStmt] = expr.match {
          case Literal(UnitConstant()) => compiledStatements ++ releaseLocalVars
          case _ if dispatch[DataStructureIFFragment](_.usesRefCount)(expr.tpe) && releaseLocalVars.nonEmpty =>
            val blockResName = ctx.uniqueValueName("block_res")
            val blockResDecl = CVarDecl(
              blockResName,
              dispatch[TypeIFFragment](_.compileTypeRepr)(expr.tpe),
              Some(retain(dispatch[TermIFFragment](_.compileTermToCExpr)(expr), expr.tpe))
            )
            compiledStatements ++
            List[CStmt](blockResDecl) ++
            (CEmptyStmt :: releaseLocalVars) ++
            List(release(blockResDecl.ref, expr.tpe, CTrueLiteral).get, blockResDecl.ref)
          case _ => compiledStatements.appended(dispatch[TermIFFragment](_.compileTermToCExpr)(expr))
        }

        CStmtExpr(CCompoundStmt(stmtList))
    }
  }

  override def compileBlockToCCompoundStmt(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Block, CCompoundStmt] = {
    import quotes.reflect.*

    {
      case Block(statements, expr) =>
        val compiledStatements = statements.map(dispatch[StatementIFFragment](_.compileStatementToCStmt))

        val stmtList = expr.match {
          case Literal(UnitConstant()) => compiledStatements
          case _ => compiledStatements.appended(dispatch[TermIFFragment](_.compileTermToCStmt)(expr))
        }

        val releaseLocalVars = DataStructureFragment.releaseLocalVars(stmtList) match {
          case Nil => Nil
          case l   => CEmptyStmt :: l
        }

        CCompoundStmt(stmtList ++ releaseLocalVars)
    }
  }

  override def compileBlockToFunctionBody(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Block, CCompoundStmt] = {
    import quotes.reflect.*

    {
      case Block(statements, expr) =>
        val compiledStatements = statements.map(dispatch[StatementIFFragment](_.compileStatementToCStmt))

        val stmtList = expr match {
          case Literal(UnitConstant()) => compiledStatements
          case _ if expr.tpe =:= TypeRepr.of[Unit] =>
            compiledStatements.appended(dispatch[TermIFFragment](_.compileTermToCStmt)(expr))
          case ret: Return => compiledStatements.appended(dispatch[TermIFFragment](_.compileReturn)(ret))
          case _ => compiledStatements.appended(CReturnStmt(Some(dispatch[TermIFFragment](_.compileTermToCExpr)(expr))))
        }

        CCompoundStmt(stmtList)
    }
  }

  override def compileIfToCIfStmt(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.If, CIfStmt] = {
    import quotes.reflect.*

    {
      case If(cond, thenp, elsep) =>
        val condExpr  = dispatch[TermIFFragment](_.compileTermToCExpr)(cond)
        val thenpStmt = dispatch[TermIFFragment](_.compileTermToCStmt)(thenp)
        val elsepStmt = elsep match {
          case Literal(UnitConstant()) => None
          case _                       => Some(dispatch[TermIFFragment](_.compileTermToCStmt)(elsep))
        }

        CIfStmt(condExpr, thenpStmt, elsepStmt)
    }
  }

  override def compileIfToCConditionalOperator(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.If, CConditionalOperator] = {
    import quotes.reflect.*

    {
      case If(cond, thenp, elsep) =>
        val condExpr  = dispatch[TermIFFragment](_.compileTermToCExpr)(cond)
        val thenpExpr = dispatch[TermIFFragment](_.compileTermToCExpr)(thenp)
        val elsepExpr = dispatch[TermIFFragment](_.compileTermToCExpr)(elsep)

        CConditionalOperator(condExpr, thenpExpr, elsepExpr)
    }
  }

  override def compileReturn(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Return, CReturnStmt] = {
    import quotes.reflect.*

    {
      case Return(Literal(UnitConstant), _) => CReturnStmt()
      case Return(term, _)                  => CReturnStmt(Some(dispatch[TermIFFragment](_.compileTermToCExpr)(term)))
    }
  }

  override def compileWhile(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.While, CWhileStmt] = {
    import quotes.reflect.*

    {
      case While(cond, body) =>
        val compiledCond = dispatch[TermIFFragment](_.compileTermToCExpr)(cond)

        val compiledBody = body match {
          case block: Block => dispatch[TermIFFragment](_.compileBlockToCCompoundStmt)(block)
          case term         => CCompoundStmt(List(dispatch[TermIFFragment](_.compileTermToCStmt)(term)))
        }

        CWhileStmt(compiledCond, compiledBody)
    }
  }
}
