package reactive

import clangast.WithContext
import clangast.decl.CFunctionDecl
import clangast.expr.CExpr
import clangast.types.CType
import macros.{ScalaToC, TranslationContext}

import scala.quoted.*

case class Fold[V](init: WithContext[CExpr], cType: WithContext[CType], lines: List[FLine[_, V]]) extends Event[V] {
  override def inputs: List[ReSource] = lines.map(_.input)

  override val baseName: String = "fold"
}

object Fold {
  def foldCode[V, R](input: Expr[Event[V]], init: Expr[R], f: Expr[(R, V) => R], funName: Expr[String])(using Quotes, Type[V], Type[R]): Expr[Fold[R]] = {
    import quotes.reflect.*

    val initCAST = ScalaToC.compileExpr(init)
    val tpeCAST = ScalaToC.compileType[R]
    val fCAST = ScalaToC.compileAnonFun(f, funName)

    '{ Fold($initCAST, $tpeCAST, List(FLine($input, $fCAST))) }
  }
}

extension [V] (inline input: Event[V])
  inline def fold[R](inline init: R)(inline funName: String = "fold")(inline f: (R, V) => R): Fold[R] =
    ${ Fold.foldCode('input, 'init, 'f, 'funName) }
