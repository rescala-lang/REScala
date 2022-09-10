package compiler

import api2.{CompiledReactive, CompiledSignalExpr, GraphCompiler}
import clangast.*
import clangast.given
import clangast.WithContext
import clangast.decl.CFunctionDecl
import clangast.expr.CIntegerLiteral
import clangast.stmt.{CCompoundStmt, CReturnStmt}
import compiler.base.*
import compiler.ext.*
import compiler.context.{ReactiveTC, RecordDeclTC}

import scala.annotation.tailrec
import scala.quoted.*

trait ReactiveMacroCompilerCode extends MacroCompilerCode {
  override given cascade: CompilerCascade = CompileReactive ~>: standardCascade

  override type CTX = StandardContext with ReactiveTC

  override protected def createTranslationContext(): CTX = new StandardContext with ReactiveTC {}

  @tailrec
  protected final def removeInlines(using Quotes)(t: quotes.reflect.Term): quotes.reflect.Term = t match {
    case quotes.reflect.Inlined(_, _, inner) => removeInlines(inner)
    case _ => t
  }

  def compileGraphCode(appName: Expr[String])(graph: Expr[_])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()

    val (params, stmts, expr) = removeInlines(graph.asTerm) match {
      case Block(_, Block(List(DefDef(_, List(TermParamClause(params)), _, Some(Block(List(_: Import), Block(stmts, expr))))), _)) => (params, stmts, expr)
      case Block(_, Block(List(DefDef(_, _, _, Some(Block(params, Block(stmts, expr))))), _)) => (params, stmts, expr)
      case Block(_, Block(List(DefDef(_, _, _, Some(Block(stmts, expr)))), _)) => (List(), stmts, expr)
      case Block(List(DefDef(_, _, _, Some(Match(_, List(CaseDef(Unapply(_, _, params), _, Block(stmts, expr))))))), _) => (params, stmts, expr)
      case Block(stmts, expr) => (List(), stmts, expr)
    }

    val outputReactives = expr match {
      case Apply(TypeApply(Select(Ident(className), "apply"), _), l) if className.startsWith("Tuple") =>
        stmts.foreach(cascade.dispatch(_.compileReactiveTopLevelStmt))

        l.collect { case i: Ident => ctx.reactivesList.find(_.name.equals(i.name)) }.flatten
      case Block(List(innerExpr), Literal(UnitConstant())) =>
        (stmts :+ innerExpr).foreach(cascade.dispatch(_.compileReactiveTopLevelStmt))

        List()
      case _ =>
        (stmts :+ expr).foreach(cascade.dispatch(_.compileReactiveTopLevelStmt))

        List()
    }

    val externalSources = params.map {
      case ValDef(name, _, _) => name
      case Bind(name, _) => name
    }.flatMap(name => ctx.reactivesList.find(_.name.equals(name)))

    val gc = new GraphCompiler(using summon[Quotes])(ctx.reactivesList, externalSources, outputReactives, appName.valueOrAbort)
    gc.writeIntoDir("out/" + appName.valueOrAbort, "gcc")

    '{()}
  }
}
