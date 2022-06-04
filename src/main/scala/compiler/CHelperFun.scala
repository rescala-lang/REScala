package compiler

import clangast.WithContext
import clangast.decl.CFunctionDecl

import scala.quoted.*

trait CHelperFun(val f: WithContext[CFunctionDecl])

class CHelperFun0[R](f: WithContext[CFunctionDecl]) extends CHelperFun(f) {
  def apply(): R = ???
}

class CHelperFun1[T1, R](f: WithContext[CFunctionDecl]) extends CHelperFun(f) {
  def apply(a1: T1): R = ???
}

class CHelperFun2[T1, T2, R](f: WithContext[CFunctionDecl]) extends CHelperFun(f) {
  def apply(a1: T1, a2: T2): R = ???
}

object CHelperFun {
  inline def apply[R, C <: MacroCompiler](inline f: () => R)(using mc: C): CHelperFun0[R] =
    new CHelperFun0(mc.compileFun(f))

  inline def apply[T1, R, C <: MacroCompiler](inline f: T1 => R)(using mc: C): CHelperFun1[T1, R] =
    new CHelperFun1(mc.compileFun(f))

  inline def apply[T1, T2, R, C <: MacroCompiler](inline f: (T1, T2) => R)(using mc: C): CHelperFun2[T1, T2, R] =
    new CHelperFun2(mc.compileFun(f))
}
