package macros

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.*
import clangast.expr.unaryop.*
import clangast.stmt.*
import clangast.traversal.CASTMapper
import clangast.types.*

import macros.CompileTree.*
import macros.CompileStatement.*
import macros.CompileDefinition.*
import macros.CompileTerm.*
import macros.CompileType.*

import scala.annotation.tailrec
import scala.quoted.*

object ScalaToC {
  def scalaToCCode[T](expr: Expr[T], funName: Expr[String])(using Quotes): Expr[CASTNode] = {
    import quotes.reflect.*

    val cast = compileTree(expr.asTerm, new TranslationContext()) match {
      case funDecl: CFunctionDecl => funDecl.copy(name = funName.value.get)
      case astNode => astNode
    }

    println(cast.textgen)

    cast.toExpr
  }

  inline def scalaToC(inline funName: String)(inline expr: Any): CASTNode = ${ scalaToCCode('expr, 'funName) }

  def compileAnonFun(f: Expr[_], funName: Expr[String])(using Quotes): Expr[WithContext[CFunctionDecl]] = {
    import quotes.reflect.*

    val ctx = new TranslationContext()

    val (originalName, compiledF) = compileTerm(f.asTerm, ctx) match {
      case funDecl: CFunctionDecl => (funDecl.name, funDecl.copy(name = funName.value.get))
    }

    WithContext(compiledF, ctx, originalName).toExpr
  }

  def compileFun(f: Expr[_])(using Quotes): Expr[WithContext[CFunctionDecl]] = {
    import quotes.reflect.*

    val ctx = new TranslationContext()

    val compiledF = compileTerm(f.asTerm, ctx) match { case funDecl: CFunctionDecl => funDecl }

    WithContext(compiledF, ctx).toExpr
  }

  def compileExpr(e: Expr[_])(using Quotes): Expr[WithContext[CExpr]] = {
    import quotes.reflect.*

    val ctx = new TranslationContext()

    val compiledExpr = compileTermToCExpr(e.asTerm, ctx)

    WithContext(compiledExpr, ctx).toExpr
  }
  
  def compileType[T](using Quotes, Type[T]): Expr[WithContext[CType]] = {
    import quotes.reflect.*

    val ctx = new TranslationContext()
    
    val compiledTypeRepr = compileTypeRepr(TypeRepr.of[T], ctx)

    WithContext(compiledTypeRepr, ctx).toExpr
  }
}
