package api

import clangast.WithContext
import clangast.decl.CFunctionDecl
import compiler.*

import scala.annotation.compileTimeOnly
import scala.quoted.*

trait CHelperFun(val f: WithContext[CFunctionDecl])

class CHelperFun0[R](f: WithContext[CFunctionDecl]) extends CHelperFun(f) {
  @compileTimeOnly("This method can only be used in expressions that are translated to C code")
  def apply(): R = ???
}

class CHelperFun1[T1, R](f: WithContext[CFunctionDecl]) extends CHelperFun(f) {
  @compileTimeOnly("This method can only be used in expressions that are translated to C code")
  def apply(a1: T1): R = ???
}

class CHelperFun2[T1, T2, R](f: WithContext[CFunctionDecl]) extends CHelperFun(f) {
  @compileTimeOnly("This method can only be used in expressions that are translated to C code")
  def apply(a1: T1, a2: T2): R = ???
}

class CHelperFun3[T1, T2, T3, R](f: WithContext[CFunctionDecl]) extends CHelperFun(f) {
  @compileTimeOnly("This method can only be used in expressions that are translated to C code")
  def apply(a1: T1, a2: T2, a3: T3): R = ???
}

class CHelperFun4[T1, T2, T3, T4, R](f: WithContext[CFunctionDecl]) extends CHelperFun(f) {
  @compileTimeOnly("This method can only be used in expressions that are translated to C code")
  def apply(a1: T1, a2: T2, a3: T3, a4: T4): R = ???
}

class CHelperFun5[T1, T2, T3, T4, T5, R](f: WithContext[CFunctionDecl]) extends CHelperFun(f) {
  @compileTimeOnly("This method can only be used in expressions that are translated to C code")
  def apply(a1: T1, a2: T2, a3: T3, a4: T4, a5: T5): R = ???
}

object CHelperFun {
  inline def apply[R, C <: MacroCompiler](inline f: () => R)(using mc: C, hfc: HelperFunCollection): CHelperFun0[R] = {
    new CHelperFun0(hfc.add(mc.compileFun(f)))
  }

  inline def apply[T1, R, C <: MacroCompiler](inline f: T1 => R)(using
      mc: C,
      hfc: HelperFunCollection
  ): CHelperFun1[T1, R] = {
    new CHelperFun1(hfc.add(mc.compileFun(f)))
  }

  inline def apply[T1, T2, R, C <: MacroCompiler](inline f: (T1, T2) => R)(using
      mc: C,
      hfc: HelperFunCollection
  ): CHelperFun2[T1, T2, R] = {
    new CHelperFun2(hfc.add(mc.compileFun(f)))
  }

  inline def apply[T1, T2, T3, R, C <: MacroCompiler](inline f: (T1, T2, T3) => R)(using
      mc: C,
      hfc: HelperFunCollection
  ): CHelperFun3[T1, T2, T3, R] = {
    new CHelperFun3(hfc.add(mc.compileFun(f)))
  }

  inline def apply[T1, T2, T3, T4, R, C <: MacroCompiler](inline f: (T1, T2, T3, T4) => R)(using
      mc: C,
      hfc: HelperFunCollection
  ): CHelperFun4[T1, T2, T3, T4, R] = {
    new CHelperFun4(hfc.add(mc.compileFun(f)))
  }

  inline def apply[T1, T2, T3, T4, T5, R, C <: MacroCompiler](inline f: (T1, T2, T3, T4, T5) => R)(using
      mc: C,
      hfc: HelperFunCollection
  ): CHelperFun5[T1, T2, T3, T4, T5, R] = {
    new CHelperFun5(hfc.add(mc.compileFun(f)))
  }
}
