package macros

import clangast.WithContext
import clangast.decl.CFunctionDecl

import scala.quoted.*

trait CHelperFun

class CHelperFun0[R](f: WithContext[CFunctionDecl]) extends CHelperFun {
  def apply(): R = ???
}

class CHelperFun1[T1, R](f: WithContext[CFunctionDecl]) extends CHelperFun {
  def apply(a1: T1): R = ???
}

class CHelperFun2[T1, T2, R](val f: WithContext[CFunctionDecl]) extends CHelperFun {
  def apply(a1: T1, a2: T2): R = ???
}

object CHelperFun {
  def helper0Code[R](f: Expr[() => R])(using Quotes, Type[R]): Expr[CHelperFun0[R]] = {
    import quotes.reflect.*

    val fCAST = ScalaToC.compileFun(f)

    '{ new CHelperFun0($fCAST) }
  }

  def helper1Code[T1, R](f: Expr[T1 => R])(using Quotes, Type[T1], Type[R]): Expr[CHelperFun1[T1, R]] = {
    import quotes.reflect.*

    val fCAST = ScalaToC.compileFun(f)

    '{ new CHelperFun1($fCAST) }
  }

  def helper2Code[T1, T2, R](f: Expr[(T1, T2) => R])(using Quotes, Type[T1], Type[T2], Type[R]): Expr[CHelperFun2[T1, T2, R]] = {
    import quotes.reflect.*

    val fCAST = ScalaToC.compileFun(f)

    '{ new CHelperFun2($fCAST) }
  }

  inline def apply[R](inline f: () => R): CHelperFun0[R] =
    ${ helper0Code('f) }

  inline def apply[T1, R](inline f: T1 => R): CHelperFun1[T1, R] =
    ${ helper1Code('f) }

  inline def apply[T1, T2, R](inline f: (T1, T2) => R): CHelperFun2[T1, T2, R] =
    ${ helper2Code('f) }
}
