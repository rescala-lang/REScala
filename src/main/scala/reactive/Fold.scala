package reactive

import clangast.decl.CFunctionDecl
import clangast.expr.CExpr
import macros.{ScalaToC, TranslationContext}

import scala.quoted.*

case class Fold[V](init: CExpr, lines: List[FLine[_, V]]) extends Event[V] {
  override def inputs: List[ReSource] = lines.map(_.input)
}

object Fold {
  def foldCode[V, R](input: Expr[Event[V]], init: Expr[R], f: Expr[(R, V) => R], funName: Expr[String])(using Quotes, Type[V], Type[R]): Expr[Fold[R]] = {
    import quotes.reflect.*

    val initCAST = ScalaToC.compileExpr(init)
    val fCAST = ScalaToC.compileAnonFun(f, funName)

    '{ Fold($initCAST, List(FLine($input, $fCAST))) }
  }
}

extension [V] (inline input: Event[V])
  inline def fold[R](inline init: R)(inline name: String = "fold")(inline f: (R, V) => R): Fold[R] =
    ${ Fold.foldCode('input, 'init, 'f, 'name) }
