package reactive

import clangast.WithContext
import clangast.decl.CFunctionDecl
import clangast.types.CType
import macros.ScalaToC

import scala.quoted.*

case class Map1[A, R](input: Event[A], cType: WithContext[CType], f: WithContext[CFunctionDecl]) extends Event[R] {
  override def inputs: List[ReSource] = List(input)

  override val baseName: String = "map"
}

object Map1 {
  def mapCode[A, R](input: Expr[Event[A]], f: Expr[A => R], funName: Expr[String])(using Quotes, Type[A], Type[R]): Expr[Map1[A, R]] = {
    import quotes.reflect.*

    val fCAST = ScalaToC.compileAnonFun(f, funName)
    val tpeCAST = ScalaToC.compileType[R]

    '{ Map1($input, $tpeCAST, $fCAST) }
  }
}

extension [A] (inline input: Event[A])
  inline def map[R](inline funName: String = "map")(inline f: A => R): Map1[A, R] =
    ${ Map1.mapCode('input, 'f, 'funName) }
    
  inline def observe(inline funName: String = "observe")(inline f: A => Unit): Map1[A, Unit] =
    ${ Map1.mapCode('input, 'f, 'funName) }
