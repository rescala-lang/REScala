package compiler

import clangast.{CASTNode, WithContext}
import clangast.decl.CFunctionDecl
import clangast.expr.CExpr
import clangast.types.CType
import compiler.context.TranslationContext
import compiler.base.*
import compiler.ext.*

import scala.quoted.*

trait MacroCompiler {
  given cascade: CompilerCascade

  type CTX <: WithContext.RequiredTC

  protected def createTranslationContext(): CTX
  
  inline def compileTree(inline t: Any): WithContext[CASTNode]
  
  protected def compileTreeCode(t: Expr[_])(using Quotes): Expr[WithContext[CASTNode]] = {
    import quotes.reflect.*
    
    given ctx: CTX = createTranslationContext()
    
    WithContext(cascade.dispatch(_.compileTree)(t.asTerm), ctx).toExpr
  }
  
  inline def compileExpr(inline e: Any): WithContext[CExpr]
  
  protected def compileExprCode(e: Expr[_])(using Quotes): Expr[WithContext[CExpr]] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()
    
    WithContext(cascade.dispatch(_.compileTermToCExpr)(e.asTerm), ctx).toExpr
  }
  
  inline def compileFun(inline f: AnyRef): WithContext[CFunctionDecl]
  
  protected def compileFunCode(f: Expr[_])(using Quotes): Expr[WithContext[CFunctionDecl]] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()

    val compiledF = cascade.dispatch(_.compileTerm)(f.asTerm) match { case funDecl: CFunctionDecl => funDecl }

    WithContext(compiledF, ctx, compiledF.name).toExpr
  }
  
  inline def compileAnonFun(inline f: AnyRef, inline funName: String): WithContext[CFunctionDecl]

  protected def compileAnonFunCode(f: Expr[_], funName: Expr[String])(using Quotes): Expr[WithContext[CFunctionDecl]] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()

    val (originalName, compiledF) = cascade.dispatch(_.compileTerm)(f.asTerm) match {
      case funDecl: CFunctionDecl => (funDecl.name, funDecl.copy(name = funName.valueOrAbort))
    }

    WithContext(compiledF, ctx, originalName).toExpr
  }

  inline def compileType[T]: WithContext[CType]
  
  protected def compileTypeCode[T](using Quotes, Type[T]): Expr[WithContext[CType]] = {
    import quotes.reflect.*

    given ctx: CTX = createTranslationContext()

    val compiledTypeRepr = cascade.dispatch(_.compileTypeRepr)(TypeRepr.of[T])

    WithContext(compiledTypeRepr, ctx).toExpr
  }

  inline def valName: String

  protected def valNameCode(using Quotes): Expr[String] = {
    import quotes.reflect.*

    Expr(Symbol.spliceOwner.owner.name)
  }
}
