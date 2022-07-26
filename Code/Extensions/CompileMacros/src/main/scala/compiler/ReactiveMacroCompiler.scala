package compiler

import clangast.{CASTNode, WithContext}
import clangast.decl.CFunctionDecl
import clangast.expr.CExpr
import clangast.types.CType
import compiler.context.ReactiveTC
import compiler.ext.CompileReactive
import rescala.api2.MetaBundle

trait ReactiveMacroCompiler extends MacroCompiler {
  inline def compileReactiveExpr(inline e: Any): WithContext[CFunctionDecl]

  inline def compileGraph(inline e: Any): Unit
}

object ReactiveMacroCompiler extends ReactiveMacroCompiler {
  private val impl: ReactiveMacroCompilerCode = new ReactiveMacroCompilerCode {
    override given cascade: CompilerCascade = CompileReactive ~>: standardCascade

    override type CTX = StandardContext with ReactiveTC

    override protected def createTranslationContext(): CTX = new StandardContext with ReactiveTC {}
  }

  export impl.*

  override inline def compileTree(inline t: Any): WithContext[CASTNode] =
    ${ compileTreeCode('t) }

  override inline def compileExpr(inline e: Any): WithContext[CExpr] =
    ${ compileExprCode('e) }

  override inline def compileFun(inline f: AnyRef): WithContext[CFunctionDecl] =
    ${ compileFunCode('f) }

  override inline def compileAnonFun(inline f: AnyRef): WithContext[CFunctionDecl] =
    ${ compileAnonFunCode('f) }

  override inline def compileType[T]: WithContext[CType] =
    ${ compileTypeCode[T] }

  override inline def valName: String =
    ${ valNameCode }

  override inline def compileReactiveExpr(inline e: Any): WithContext[CFunctionDecl] =
    ${ compileReactiveExprCode('e) }

  override inline def compileGraph(inline graph: Any): Unit =
    ${ compileGraphCode('graph) }
}
