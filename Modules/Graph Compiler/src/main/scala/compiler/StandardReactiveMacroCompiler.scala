package compiler

import clangast.{CASTNode, WithContext}
import clangast.decl.CFunctionDecl
import clangast.expr.CExpr
import clangast.types.CType

object StandardReactiveMacroCompiler extends ReactiveMacroCompiler {
  private val impl: ReactiveMacroCompilerCode = new ReactiveMacroCompilerCode {}

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

  override inline def compileGraph(inline appName: String)(inline graph: Any): Unit =
    ${ compileGraphCode('appName)('graph) }
}
