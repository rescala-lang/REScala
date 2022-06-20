package compiler

import clangast.decl.CFunctionDecl
import clangast.expr.CExpr
import clangast.types.CType
import clangast.{CASTNode, WithContext}
import compiler.base.*
import compiler.context.TranslationContext

import scala.quoted.*

trait MacroCompilerCode {
  protected given cascade: CompilerCascade

  protected type CTX <: TranslationContext

  protected def createTranslationContext(): CTX

  def compileTreeCode(t: Expr[_])(using Quotes): Expr[WithContext[CASTNode]] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()

    WithContext(cascade.dispatch(_.compileTree)(t.asTerm), ctx).toExpr
  }

  def compileExprCode(e: Expr[_])(using Quotes): Expr[WithContext[CExpr]] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()

    WithContext(cascade.dispatch(_.compileTermToCExpr)(e.asTerm), ctx).toExpr
  }

  def compileFunCode(f: Expr[_])(using Quotes): Expr[WithContext[CFunctionDecl]] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()

    val (originalName, compiledF) = cascade.dispatch(_.compileTerm)(f.asTerm) match {
      case funDecl: CFunctionDecl => (funDecl.name, funDecl.copy(name = Symbol.spliceOwner.owner.name))
    }

    WithContext(
      compiledF,
      ctx,
      { case CFunctionDecl(`originalName`, _, _, _, _) => true }
    ).toExpr
  }

  def compileAnonFunCode(f: Expr[_])(using Quotes): Expr[WithContext[CFunctionDecl]] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()

    val compiledF = cascade.dispatch(_.compileTerm)(f.asTerm) match { case funDecl: CFunctionDecl => funDecl }

    WithContext(
      compiledF,
      ctx,
      { case CFunctionDecl(compiledF.name, _, _, _, _) => true }
    ).toExpr
  }

  def compileTypeCode[T](using Quotes, Type[T]): Expr[WithContext[CType]] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()
    
    // make sure that release and retain for the given type are compiled for later use
    cascade.dispatchLifted(_.compileRetain)(TypeRepr.of[T])
    cascade.dispatchLifted(_.compileRelease)(TypeRepr.of[T])

    val compiledTypeRepr = cascade.dispatch(_.compileTypeRepr)(TypeRepr.of[T])

    WithContext(compiledTypeRepr, ctx).toExpr
  }

  def valNameCode(using Quotes): Expr[String] = {
    import quotes.reflect.*

    Expr(Symbol.spliceOwner.owner.name)
  }
}
