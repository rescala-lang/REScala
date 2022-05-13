package reactive

import clangast.decl.CFunctionDecl
import macros.ScalaToC

import scala.quoted.*

case class Map1[A, R](input: Event[A], f: CFunctionDecl) extends Event[R] {
  override def inputs: List[ReSource] = List(input)
}

object Map1 {
  def mapCode[A, R](input: Expr[Event[A]], f: Expr[A => R], funName: Expr[String])(using Quotes, Type[A], Type[R]): Expr[Map1[A, R]] = {
    import quotes.reflect.*

    val cast = ScalaToC.compileAnonFun(f, funName)

    '{ Map1($input, $cast) }
  }
}

extension [A] (inline input: Event[A]) inline def map[R](inline name: String = "map")(inline f: A => R): Map1[A, R] =
  ${ Map1.mapCode('input, 'f, 'name) }
