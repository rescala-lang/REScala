package compiler.ext

import api2.*
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
import rescala.macros.MacroAccess

import scala.quoted.*

object ReactiveFragment extends SelectIFFragment with ApplyIFFragment with ReactiveIFFragment {
  override def compileSelect(using Quotes)(using FragmentedCompiler)(using TranslationContext):
    PartialFunction[quotes.reflect.Select, CExpr] = ensureCtx[ReactiveTC] { ctx ?=>
      import quotes.reflect.*

      {
        case this.reactiveValueAccess(rName, tpe) =>
          val (rParam, _) = ctx.inputParameters.getOrElseUpdate(
            rName,
            (CParmVarDecl(rName, dispatch[TypeIFFragment](_.compileTypeRepr)(tpe)), tpe.asInstanceOf[Any])
          )

          rParam.ref
      }
    }

  private def reactiveValueAccess(using Quotes): PartialFunction[quotes.reflect.Term, (String, quotes.reflect.TypeRepr)] = {
    import quotes.reflect.*

    {
      case select@Select(r@Ident(rName), "value") if r.tpe <:< TypeRepr.of[CReactive[_]] =>
        (rName, select.tpe)
      case select@Select(Inlined(_, _, r@Ident(rName)), "value") if r.tpe <:< TypeRepr.of[CReactive[_]] =>
        (rName, select.tpe)
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
        case ValDef(name, tpt, Some(rhs)) if tpt.tpe <:< TypeRepr.of[CReactive[_]] =>
          ctx.addReactive(dispatch[ReactiveIFFragment](_.compileReactive)(rhs).rename(name))
        case t: Term if t.tpe <:< TypeRepr.of[CReactive[_]] =>
          ctx.addReactive(dispatch[ReactiveIFFragment](_.compileReactive)(t))
        case valDef: ValDef =>
          val decl = dispatch[DefinitionIFFragment](_.compileValDefToCVarDecl)(valDef)
          ctx.addValueDecl(decl)
        case defDef: DefDef =>
          val decl = dispatch[DefinitionIFFragment](_.compileDefDef)(defDef)
          ctx.nameToFunctionDecl.put(decl.name, decl)
        case _ =>
      }
    }

  override def compileReactive(using Quotes)(using FragmentedCompiler)(using ctx: TranslationContext):
    PartialFunction[quotes.reflect.Term, CompiledReactive] = ensureCtx[ReactiveTC] {
      import quotes.reflect.*

      {
        case inlined@Inlined(Some(Apply(Apply(TypeApply(apply@Apply(TypeApply(Ident("fold"), _), List(Ident(inputReactive))), _), List(init)), List(f))), _, _)
          if inlined.tpe <:< TypeRepr.of[CReactive[_]] =>
          val name = "signal" + posToString(apply.pos)

          val cFun = dispatch[ReactiveIFFragment](_.compileReactiveExpr)(f)
          val cInit = dispatch[TermIFFragment](_.compileTermToCExpr)(init)

          CompiledFold(name, cInit, inputReactive, cFun, init.tpe)
        case r @ this.reactiveExpression(id, expr) =>
          val posString = posToString(r.pos)

          val cFun = dispatch[ReactiveIFFragment](_.compileReactiveExpr)(expr)

          if id.equals("CSignal") then CompiledSignalExpr("signal" + posString, cFun, expr.tpe)
          else CompiledEvent("event" + posString, cFun, expr.tpe)
        case TypeApply(sel@Select(Ident("CEvent"), "source"), List(tpt)) =>
          val optType = TypeRepr.of[Option].appliedTo(tpt.tpe)
          val cFun = CFunctionDecl("dummy", List(), dispatch[TypeIFFragment](_.compileTypeRepr)(optType))

          CompiledEvent("event" + posToString(sel.pos), cFun, optType)
        case apply@Apply(TypeApply(Select(Ident("CSignal"), "source"), _), List(init)) =>
          val name = "signal" + posToString(apply.pos)
          val cFun = CFunctionDecl(
            "init_" + name,
            List(),
            dispatch[TypeIFFragment](_.compileTypeRepr)(init.tpe),
            Some(CCompoundStmt(List(CReturnStmt(Some(
              dispatch[TermIFFragment](_.compileTermToCExpr)(init)
            )))))
          )

          CompiledSignalExpr(name, cFun, init.tpe)
        case this.inlinedExtensionCall(pos, ext, id, expr) =>
          val posString = posToString(pos)

          val fName = "anonfun_" + pos.sourceFile.name.stripSuffix(".scala") + posString
          val cFun = dispatch[ReactiveIFFragment](_.compileReactiveExpr)(expr).copy(name = fName)

          if id.equals("CSignal") then CompiledSignalExpr("signal" + posString, cFun, expr.tpe)
          else CompiledEvent("event" + posString, cFun, expr.tpe, ext.equals("map"))
      }
    }

  private def posToString(using Quotes)(pos: quotes.reflect.Position): String =
    "_" + (pos.startLine + 1) + "_" + (pos.startColumn + 1)

  private def inlinedExtensionCall(using Quotes): PartialFunction[
    quotes.reflect.Term,
    (quotes.reflect.Position, String, String, quotes.reflect.Term)
  ] = term => {
    import quotes.reflect.*

    term match {
      case inlined@Inlined(Some(Apply(TypeApply(Apply(TypeApply(extId@Ident(ext), _), _), _), _)), _, Typed(this.reactiveExpression(id, expr), _))
        if inlined.tpe <:< TypeRepr.of[CReactive[_]] => (extId.pos, ext, id, expr)
      case inlined@Inlined(Some(Apply(Apply(TypeApply(extId@Ident(ext), _), _), _)), _, Typed(this.reactiveExpression(id, expr), _))
        if inlined.tpe <:< TypeRepr.of[CReactive[_]] => (extId.pos, ext, id, expr)
      case inlined@Inlined(Some(Apply(TypeApply(extId@Ident(ext), _), _)), _, Typed(this.reactiveExpression(id, expr), _))
        if inlined.tpe <:< TypeRepr.of[CReactive[_]] => (extId.pos, ext, id, expr)
    }
  }

  private def reactiveExpression(using Quotes): PartialFunction[quotes.reflect.Term, (String, quotes.reflect.Term)] = {
    import quotes.reflect.*

    {
      case apply@Apply(TypeApply(Select(Ident(id @ ("CSignal" | "CEvent")), "apply"), _), List(expr))
        if apply.tpe <:< TypeRepr.of[CReactive[_]] => (id, expr)
    }
  }

  override def compileReactiveExpr(using Quotes)(using FragmentedCompiler)(using TranslationContext):
    PartialFunction[quotes.reflect.Term, CFunctionDecl] = ensureCtx[ReactiveTC] { ctx ?=>
      import quotes.reflect.*

      {
        case Block(List(), expr) =>
          dispatch[ReactiveIFFragment](_.compileReactiveExpr)(expr)
        case Block(List(f: DefDef), Closure(_, _)) =>
          val cFun = dispatch[DefinitionIFFragment](_.compileDefDef)(f)

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
          val inferredName = "anonfun_" + pos.sourceFile.name.stripSuffix(".scala") + posToString(pos)

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
