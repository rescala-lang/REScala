package reactive

import clangast.decl.CFunctionDecl
import clangast.types.CType
import macros.ScalaToC

import scala.quoted.*

case class Map2[A, B, R](l: Event[A], r: Event[B], cType: CType, f: CFunctionDecl) extends Event[R] {
  override def inputs: List[ReSource] = List(l, r)

  override val baseName: String = "map2"
}

object Map2 {
  def map2Code[A, B, R](
                         l: Expr[Event[A]],
                         r: Expr[Event[B]],
                         f: Expr[(A, B) => R],
                         funName: Expr[String]
                       )
                       (
                         using Quotes, Type[A], Type[B], Type[R]
                       ): Expr[Map2[A, B, R]] = {
    import quotes.reflect.*

    val fCAST = ScalaToC.compileAnonFun(f, funName)
    val tpeCAST = ScalaToC.compileType[R]

    '{ Map2($l, $r, $tpeCAST, $fCAST) }
  }
}

extension[A] (inline l: Event[A]) inline def map2[B, R](inline r: Event[B])(inline funName: String = "map2")(inline f: (A, B) => R): Map2[A, B, R] =
  ${ Map2.map2Code('l, 'r, 'f, 'funName) }