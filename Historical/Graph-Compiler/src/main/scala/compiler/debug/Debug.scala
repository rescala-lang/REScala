package compiler.debug

import scala.annotation.compileTimeOnly

object Debug {
  @compileTimeOnly("This method can only be used in expressions that are translated to C code")
  def showAST[T](t: T): T = ???

  @compileTimeOnly("This method can only be used in expressions that are translated to C code")
  def showType[T](t: T): T = ???

  @compileTimeOnly("This method can only be used in expressions that are translated to C code")
  def showASTandType[T](t: T): T = ???

  @compileTimeOnly("This method can only be used in expressions that are translated to C code")
  def showCompiled[T](t: T): T = ???
}
