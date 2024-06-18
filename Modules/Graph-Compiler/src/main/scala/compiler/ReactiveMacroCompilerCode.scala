package compiler

import api2.{CompiledEvent, CompiledReactive, GraphCompiler}
import clangast.*
import clangast.given
import clangast.decl.CFunctionDecl
import compiler.FragmentedCompiler.dispatch
import compiler.base.*
import compiler.base.TypeFragment.typeArgs
import compiler.ext.*
import compiler.context.{ReactiveTC}

import scala.annotation.tailrec
import scala.quoted.*

trait ReactiveMacroCompilerCode extends MacroCompilerCode {
  override given compiler: FragmentedCompiler = ReactiveFragment +: standardFragmentedCompiler

  override type CTX = StandardContext & ReactiveTC

  override protected def createTranslationContext(): CTX = new StandardContext with ReactiveTC {}

  @tailrec
  protected final def removeInlines(using Quotes)(t: quotes.reflect.Term): quotes.reflect.Term = t match {
    case quotes.reflect.Inlined(_, _, inner) => removeInlines(inner)
    case _                                   => t
  }

  def compileGraphCode(appName: Expr[String])(graph: Expr[?])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()

    val (params, stmts, expr) = removeInlines(graph.asTerm) match {
      case Block(
            _,
            Block(
              List(DefDef(_, List(TermParamClause(params)), _, Some(Block(List(_: Import), Block(stmts, expr))))),
              _
            )
          ) => (params, stmts, expr)
      case Block(_, Block(List(DefDef(_, _, _, Some(Block(params, Block(stmts, expr))))), _)) => (params, stmts, expr)
      case Block(_, Block(List(DefDef(_, _, _, Some(Block(stmts, expr)))), _))                => (List(), stmts, expr)
      case Block(
            List(DefDef(_, _, _, Some(Match(_, List(CaseDef(Unapply(_, _, params), _, Block(stmts, expr))))))),
            _
          ) => (params, stmts, expr)
      case Block(stmts, expr) => (List(), stmts, expr)
    }

    val externalSources = params.map {
      case ValDef(name, tpt, _)        => (name, tpt.tpe)
      case Bind(name, wc @ Wildcard()) => (name, wc.tpe)
    } map { (name, tpe) =>
      val typeArgs(List(innerType)) = tpe.widen: @unchecked
      val optType                   = TypeRepr.of[Option].appliedTo(innerType)
      CompiledEvent(name, CFunctionDecl("", List(), dispatch[TypeIFFragment](_.compileTypeRepr)(optType)), optType)
    }

    externalSources.foreach(ctx.addReactive)

    val outputReactives = expr match {
      case Apply(TypeApply(Select(Ident(className), "apply"), _), l) if className.startsWith("Tuple") =>
        stmts.foreach(dispatch[ReactiveIFFragment](_.compileReactiveTopLevelStmt))

        l.collect { case i: Ident => ctx.reactivesList.find(_.name.equals(i.name)) }.flatten
      case Block(List(innerExpr), Literal(UnitConstant())) =>
        (stmts :+ innerExpr).foreach(dispatch[ReactiveIFFragment](_.compileReactiveTopLevelStmt))

        List()
      case _ =>
        (stmts :+ expr).foreach(dispatch[ReactiveIFFragment](_.compileReactiveTopLevelStmt))

        List()
    }

    val gc = new GraphCompiler(ctx.reactivesList, externalSources, outputReactives, appName.valueOrAbort)

    // TODO: reenable 
    //report.info(s"original code is trying to write some files, disabled for now, but reenable when working with this")
    // gc.writeIntoDir("target/graphCompile/" + appName.valueOrAbort, "gcc")

    '{ () }
  }
}
