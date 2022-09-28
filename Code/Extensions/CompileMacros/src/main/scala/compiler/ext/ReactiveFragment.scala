package compiler.ext

import api2.{CompiledEvent, CompiledFold, CompiledReactive, CompiledSignalExpr}
import clangast.*
import clangast.given
import clangast.decl.{CFunctionDecl, CParmVarDecl}
import clangast.expr.{CCallExpr, CExpr, CIntegerLiteral, CMemberExpr}
import clangast.stmt.{CCompoundStmt, CReturnStmt, CStmt}
import clangast.types.CVoidType
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.base.*
import compiler.context.{FunctionDeclTC, ReactiveTC, TranslationContext, ValueDeclTC}
import rescala.api2.CompilerReactiveMarker
import rescala.macros.MacroAccess

import scala.quoted.*

object ReactiveFragment extends SelectIFFragment with ApplyIFFragment with ReactiveIFFragment {
  override def compileSelect(using Quotes)(using FragmentedCompiler)(using TranslationContext):
    PartialFunction[quotes.reflect.Select, CExpr] = ensureCtx[ReactiveTC] { ctx ?=>
      import quotes.reflect.*

      {
        case this.eventValueAccess(eventName, tpe) =>
          val (eventParam, _) = ctx.inputParameters.getOrElseUpdate(
            eventName,
            (CParmVarDecl(eventName, dispatch[TypeIFFragment](_.compileTypeRepr)(tpe)), tpe.asInstanceOf[Object])
          )

          eventParam.ref
      }
    }

  private def eventValueAccess(using Quotes): PartialFunction[quotes.reflect.Term, (String, quotes.reflect.TypeRepr)] = {
    import quotes.reflect.*

    {
      case select@Select(event@Ident(eventName), "value") if event.tpe <:< TypeRepr.of[MacroAccess[?, ?]] =>
        (eventName, select.tpe)
      case select@Select(Inlined(_, _, event@Ident(eventName)), "value") if event.tpe <:< TypeRepr.of[MacroAccess[?, ?]] =>
        (eventName, select.tpe)
    }
  }

  override def compileApply(using Quotes)(using FragmentedCompiler)(using TranslationContext):
    PartialFunction[quotes.reflect.Apply, CExpr] = ensureCtx[FunctionDeclTC] { ctx ?=>
      import quotes.reflect.*

      {
        case Apply(Ident(funName), args) if ctx.nameToFunctionDecl.contains(funName) =>
          val compiledArgs = args.map(dispatch[TermIFFragment](_.compileTermToCExpr))
          CCallExpr(
            ctx.nameToFunctionDecl(funName).ref,
            compiledArgs
          )
      }
    }

  override def compileReactiveTopLevelStmt(using Quotes)(using fc: FragmentedCompiler)(using TranslationContext):
    PartialFunction[quotes.reflect.Statement, Unit] = ensureCtx[ReactiveTC with ValueDeclTC with FunctionDeclTC] { ctx ?=>
      import quotes.reflect.*

      {
        case ValDef(name, tpt, Some(rhs)) if tpt.tpe <:< TypeRepr.of[CompilerReactiveMarker] =>
          ctx.addReactive(dispatch[ReactiveIFFragment](_.compileReactive)(rhs).rename(name))
        case evt: Term if fc.dispatchLifted[ReactiveIFFragment](_.compileReactive)(evt).isDefined =>
          ctx.addReactive(dispatch[ReactiveIFFragment](_.compileReactive)(evt))
        case valDef: ValDef =>
          val decl = dispatch[DefinitionIFFragment](_.compileValDefToCVarDecl)(valDef)
          ctx.addValueDecl(decl)
          ctx.nameToDecl.put(decl.name, decl)
        case defDef: DefDef =>
          val decl = dispatch[DefinitionIFFragment](_.compileDefDef)(defDef)
          ctx.nameToFunctionDecl.put(decl.name, decl)
        case _ =>
      }
    }

  override def compileReactive(using Quotes)(using FragmentedCompiler)(using TranslationContext):
    PartialFunction[quotes.reflect.Term, CompiledReactive] = ensureCtx[ReactiveTC] {
      import quotes.reflect.*

      {
        case inlined@Inlined(Some(Apply(Apply(Apply(TypeApply(apply@Apply(TypeApply(Ident("fold"), _), List(Ident(primaryInput))), _), List(init)), List(f)), _)), _, _)
          if inlined.tpe <:< TypeRepr.of[CompilerReactiveMarker] =>
          val name = "signal_" + (apply.pos.startLine + 1) + "_" + (apply.pos.startColumn + 1)

          val cFun = dispatch[ReactiveIFFragment](_.compileReactiveExpr)(f)
          val cInit = dispatch[TermIFFragment](_.compileTermToCExpr)(init)

          CompiledFold(name, cInit, primaryInput, cFun, init.tpe)
        case inlined@Inlined(Some(Apply(Apply(TypeApply(Ident(id), _), List(expr)), _)), _, _)
          if List("Signal", "Event").contains(id) && inlined.tpe <:< TypeRepr.of[CompilerReactiveMarker] =>
          val posString = "_" + (inlined.pos.startLine + 1) + "_" + (inlined.pos.startColumn + 1)

          val cFun = dispatch[ReactiveIFFragment](_.compileReactiveExpr)(expr)

          if id.equals("Signal") then CompiledSignalExpr("signal" + posString, cFun, expr.tpe)
          else CompiledEvent("event" + posString, cFun, expr.tpe)
        case this.inlinedExtensionCall(pos, ext, f, id, expr) =>
          val posString = "_" + (pos.startLine + 1) + "_" + (pos.startColumn + 1)

          val fName = "anonfun_" + f.pos.sourceFile.name.stripSuffix(".scala") + "_" + (f.pos.startLine + 1) + "_" + (f.pos.startColumn + 1)
          val cFun = dispatch[ReactiveIFFragment](_.compileReactiveExpr)(expr).copy(name = fName)

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
      case inlined@Inlined(Some(Apply(TypeApply(apply@Apply(TypeApply(Ident(ext), _), _), _), List(f))), _, this.reactiveExpression(id, expr))
        if List("Signal", "Event").contains(id) && inlined.tpe <:< TypeRepr.of[CompilerReactiveMarker] => (apply.pos, ext, f, id, expr)
      case inlined@Inlined(Some(Apply(apply@Apply(TypeApply(Ident(ext), _), _), List(f))), _, this.reactiveExpression(id, expr))
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

  override def compileReactiveExpr(using Quotes)(using FragmentedCompiler)(using TranslationContext):
    PartialFunction[quotes.reflect.Term, CFunctionDecl] = ensureCtx[ReactiveTC] { ctx ?=>
      import quotes.reflect.*

      {
        case Block(List(), Block(List(f: DefDef), Closure(_, _))) =>
          val cFun = dispatch[DefinitionIFFragment](_.compileDefDef)(f)

          ctx.createMissingSources()

          val fullParams = cFun.parameters ++ ctx.inputParameters.values.map(_._1).toList
          ctx.inputParameters.clear()

          cFun.copy(parameters = fullParams)
        case term =>
          val compiledBlock = term match {
            case block: Block =>
              dispatch[TermIFFragment](_.compileBlockToFunctionBody)(block)
            case _ =>
              val compiledTerm = dispatch[TermIFFragment](_.compileTermToCExpr)(term)
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
            dispatch[TypeIFFragment](_.compileTypeRepr)(term.tpe),
            Some(compiledBlock)
          )
      }
    }
}
