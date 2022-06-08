package compiler.ext

import clangast.expr.CExpr
import clangast.{CASTNode, given}
import clangast.stmt.CStmt
import compiler.{CompilerCascade, PartialCompiler, TranslationContext}
import compiler.base.CompileApply.varArgs

import scala.quoted.*

object CompileMainFunction extends PartialCompiler {
  override def compileTerm(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
  PartialFunction[quotes.reflect.Term, CASTNode] = {
    import quotes.reflect.*

    {
      case Apply(Select(Ident("CMainFunction"), "startTransaction"), varArgs(assignments)) =>
        CTransactionStatement(assignments.map(compileSourceAssignment))
    }
  }

  private def compileSourceAssignment(using Quotes)(assignment: quotes.reflect.Term)(using ctx: TranslationContext, cascade: CompilerCascade): (String, CExpr) = {
    import quotes.reflect.*

    assignment match {
      case Apply(Select(Ident(sourceName), ":="), List(v)) =>
        (sourceName, cascade.compileTermToCExpr(v))
    }
  }

}
