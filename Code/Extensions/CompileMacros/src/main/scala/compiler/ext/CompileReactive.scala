package compiler.ext

import api2.{CompiledEvent, CompiledFold, CompiledReactive, CompiledSignalExpr}
import clangast.*
import clangast.given
import clangast.decl.{CFunctionDecl, CParmVarDecl}
import clangast.expr.{CCallExpr, CExpr, CIntegerLiteral, CMemberExpr}
import clangast.stmt.{CCompoundStmt, CReturnStmt, CStmt}
import clangast.types.CVoidType
import compiler.CompilerCascade
import compiler.base.*
import compiler.context.{FunctionDeclTC, ReactiveTC, TranslationContext, ValueDeclTC}
import rescala.api2.CompilerReactiveMarker
import rescala.macros.MacroAccess

import scala.quoted.*

object CompileReactive extends SelectPC with ApplyPC with ReactivePC {
  private def compileSelectImpl(using Quotes)(using ctx: ReactiveTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = {
      import quotes.reflect.*

      {
        case this.eventValueAccess(eventName, tpe) =>
          val (eventParam, _) = ctx.inputParameters.getOrElseUpdate(
            eventName,
            (CParmVarDecl(eventName, cascade.dispatch(_.compileTypeRepr)(tpe)), tpe.asInstanceOf[Object])
          )

          eventParam.ref
      }
    }

  private def eventValueAccess(using Quotes): PartialFunction[quotes.reflect.Term, (String, quotes.reflect.TypeRepr)] = {
    import quotes.reflect.*

    {
      case select @ Select(event @ Ident(eventName), "value") if event.tpe <:< TypeRepr.of[MacroAccess[?, ?]] =>
        (eventName, select.tpe)
      case select @ Select(Inlined(_, _, event @ Ident(eventName)), "value") if event.tpe <:< TypeRepr.of[MacroAccess[?, ?]] =>
        (eventName, select.tpe)
    }
  }

  override def compileSelect(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Select, CExpr] = ensureCtx[ReactiveTC](compileSelectImpl)

  private def compileApplyImpl(using Quotes)(using ctx: FunctionDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = {
      import quotes.reflect.*

      {
        case Apply(Ident(funName), args) if ctx.nameToFunctionDecl.contains(funName) =>
          val compiledArgs = args.map(cascade.dispatch(_.compileTermToCExpr))
          CCallExpr(
            ctx.nameToFunctionDecl(funName).ref,
            compiledArgs
          )
      }
    }

  override def compileApply(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[FunctionDeclTC](compileApplyImpl)

  private def compileReactiveTopLevelStmtImpl(using Quotes)(using ctx: ReactiveTC with ValueDeclTC with FunctionDeclTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Statement, Unit] = {
      import quotes.reflect.*

      {
        case ValDef(name, tpt, Some(rhs)) if tpt.tpe <:< TypeRepr.of[CompilerReactiveMarker] =>
          ctx.addReactive(cascade.dispatch(_.compileReactive)(rhs).rename(name))
        case evt: Term if cascade.dispatchLifted(_.compileReactive)(evt).isDefined =>
          ctx.addReactive(cascade.dispatch(_.compileReactive)(evt))
        case valDef: ValDef =>
          val decl = cascade.dispatch(_.compileValDefToCVarDecl)(valDef)
          ctx.addValueDecl(decl)
          ctx.nameToDecl.put(decl.name, decl)
        case defDef: DefDef =>
          val decl = cascade.dispatch(_.compileDefDef)(defDef)
          ctx.nameToFunctionDecl.put(decl.name, decl)
        case _ =>
      }
    }

  override def compileReactiveTopLevelStmt(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Statement, Unit] = ensureCtx[ReactiveTC with ValueDeclTC with FunctionDeclTC](compileReactiveTopLevelStmtImpl)

  private def compileReactiveImpl(using Quotes)(using ctx: ReactiveTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Term, CompiledReactive] = {
      import quotes.reflect.*

      {
        case inlined @ Inlined(Some(Apply(Apply(Apply(TypeApply(apply @ Apply(TypeApply(Ident("fold"), _), List(Ident(primaryInput))), _), List(init)), List(f)), _)), _, _)
          if inlined.tpe <:< TypeRepr.of[CompilerReactiveMarker] =>
          val name = "signal_" + (apply.pos.startLine + 1) + "_" + (apply.pos.startColumn + 1)

          val cFun = cascade.dispatch(_.compileReactiveExpr)(f)
          val cInit = cascade.dispatch(_.compileTermToCExpr)(init)

          CompiledFold(name, cInit, primaryInput, cFun, init.tpe)
        case inlined @ Inlined(Some(Apply(Apply(TypeApply(Ident(id), _), List(expr)), _)), _, _)
          if List("Signal", "Event").contains(id) && inlined.tpe <:< TypeRepr.of[CompilerReactiveMarker] =>
          val posString = "_" + (inlined.pos.startLine + 1) + "_" + (inlined.pos.startColumn + 1)

          val cFun = cascade.dispatch(_.compileReactiveExpr)(expr)

          if id.equals("Signal") then CompiledSignalExpr("signal" + posString, cFun, expr.tpe)
          else CompiledEvent("event" + posString, cFun, expr.tpe)
        case this.inlinedExtensionCall(pos, ext, f, id, expr) =>
          val posString = "_" + (pos.startLine + 1) + "_" + (pos.startColumn + 1)

          val fName = "anonfun_" + f.pos.sourceFile.name.stripSuffix(".scala") + "_" + (f.pos.startLine + 1) + "_" + (f.pos.startColumn + 1)
          val cFun = cascade.dispatch(_.compileReactiveExpr)(expr).copy(name = fName)

          if id.equals("Signal") then CompiledSignalExpr("signal" + posString, cFun, expr.tpe)
          else CompiledEvent("event" + posString, cFun, expr.tpe, ext.equals("map"))
      }
    }

  private def inlinedExtensionCall(using Quotes): PartialFunction[
    quotes.reflect.Term,
    (quotes.reflect.Position, String, quotes.reflect.Term, String, quotes.reflect.Term)
  ] = term => {
    import quotes.reflect.*

    term match {
      case inlined @ Inlined(Some(Apply(TypeApply(apply @ Apply(TypeApply(Ident(ext), _), _), _), List(f))), _, this.reactiveExpression(id, expr))
        if List("Signal", "Event").contains(id) && inlined.tpe <:< TypeRepr.of[CompilerReactiveMarker] => (apply.pos, ext, f, id, expr)
      case inlined @ Inlined(Some(Apply(apply @ Apply(TypeApply(Ident(ext), _), _), List(f))), _, this.reactiveExpression(id, expr))
        if List("Signal", "Event").contains(id) && inlined.tpe <:< TypeRepr.of[CompilerReactiveMarker] => (apply.pos, ext, f, id, expr)
    }
  }

  private def reactiveExpression(using Quotes): PartialFunction[quotes.reflect.Term, (String, quotes.reflect.Term)] = {
    import quotes.reflect.*

    {
      case Typed(Inlined(Some(Apply(Apply(TypeApply(Ident(id), _), List(expr)), _)), _, _), _)
        if List("Signal", "Event").contains(id) => (id, expr)
      case Typed(Inlined(Some(Apply(Apply(TypeApply(Select(_, id), _), List(expr)), _)), _, _), _)
        if List("Signal", "Event").contains(id) => (id, expr)
    }
  }

  override def compileReactive(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CompiledReactive] = ensureCtx[ReactiveTC](compileReactiveImpl)

  private def compileReactiveExprImpl(using Quotes)(using ctx: ReactiveTC, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Term, CFunctionDecl] = {
      import quotes.reflect.*

      {
        case Block(List(), Block(List(f: DefDef), Closure(_, _))) =>
          val cFun = cascade.dispatch(_.compileDefDef)(f)

          ctx.createMissingSources()

          val fullParams = cFun.parameters ++ ctx.inputParameters.values.map(_._1).toList
          ctx.inputParameters.clear()

          cFun.copy(parameters = fullParams)
        case term =>
          val compiledBlock = term match {
            case block: Block =>
              cascade.dispatch(_.compileBlockToFunctionBody)(block)
            case _ =>
              val compiledTerm = cascade.dispatch(_.compileTermToCExpr)(term)
              CCompoundStmt(List(CReturnStmt(Some(compiledTerm))))
          }

          val pos = term.pos
          val inferredName =
            "anonfun_" + pos.sourceFile.name.stripSuffix(".scala") + "_" + (pos.startLine + 1) + "_" + (pos.startColumn + 1)

          ctx.createMissingSources()

          val params = ctx.inputParameters.values.map(_._1).toList
          ctx.inputParameters.clear()

          CFunctionDecl(
            inferredName,
            params,
            cascade.dispatch(_.compileTypeRepr)(term.tpe),
            Some(compiledBlock)
          )
      }
    }

  override def compileReactiveExpr(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Term, CFunctionDecl] = ensureCtx[ReactiveTC](compileReactiveExprImpl)
}
