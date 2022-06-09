package compiler

import clangast.{CASTNode, WithContext}
import clangast.decl.CFunctionDecl
import clangast.expr.CExpr
import clangast.types.CType

import scala.quoted.*

object StandardMacroCompiler extends MacroCompiler {
  override given cascade: CompilerCascade = standardCascade

  override type CTX = StandardContext

  override protected def createTranslationContext(): CTX = createStandardContext()
  
  override inline def compileTree(inline t: Any): WithContext[CASTNode] =
    ${ compileTreeCode('t) }

  override protected def compileTreeCode(t: Expr[_])(using Quotes): Expr[WithContext[CASTNode]] =
    super.compileTreeCode(t)

  override inline def compileExpr(inline e: Any): WithContext[CExpr] =
    ${ compileExprCode('e) }

  override protected def compileExprCode(e: Expr[_])(using Quotes): Expr[WithContext[CExpr]] =
    super.compileExprCode(e)

  override inline def compileFun(inline f: AnyRef): WithContext[CFunctionDecl] =
    ${ compileFunCode('f) }

  override protected def compileFunCode(f: Expr[_])(using Quotes): Expr[WithContext[CFunctionDecl]] =
    super.compileFunCode(f)

  override inline def compileAnonFun(inline f: AnyRef, inline funName: String): WithContext[CFunctionDecl] =
    ${ compileAnonFunCode('f, 'funName) }

  override protected def compileAnonFunCode(f: Expr[_], funName: Expr[String])(using Quotes): Expr[WithContext[CFunctionDecl]] =
    super.compileAnonFunCode(f, funName)

  override inline def compileType[T]: WithContext[CType] =
    ${ compileTypeCode[T] }

  override protected def compileTypeCode[T](using Quotes, Type[T]): Expr[WithContext[CType]] =
    super.compileTypeCode[T]

  override inline def valName: String =
    $ { valNameCode }

  override protected def valNameCode(using Quotes): Expr[String] = super.valNameCode
}
