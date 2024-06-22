package compiler

import clangast.{CASTNode, WithContext}
import clangast.decl.CFunctionDecl
import clangast.expr.CExpr
import clangast.types.CType

import scala.quoted.*

object StandardMacroCompiler extends MacroCompiler {
  private val impl: MacroCompilerCode = new MacroCompilerCode {
    override given compiler: FragmentedCompiler = standardFragmentedCompiler

    override type CTX = StandardContext

    override protected def createTranslationContext(): CTX = createStandardContext()
  }

  export impl.*

  override inline def compileTree(inline t: Any): WithContext[CASTNode] =
    ${ compileTreeCode('{ t }) }

  override inline def compileExpr(inline e: Any): WithContext[CExpr] =
    ${ compileExprCode('{ e }) }

  override inline def compileFun(inline f: AnyRef): WithContext[CFunctionDecl] =
    ${ compileFunCode('{ f }) }

  override inline def compileAnonFun(inline f: AnyRef): WithContext[CFunctionDecl] =
    ${ compileAnonFunCode('{ f }) }

  override inline def compileType[T]: WithContext[CType] =
    ${ compileTypeCode[T] }

  override inline def valName: String =
    ${ valNameCode }
}
