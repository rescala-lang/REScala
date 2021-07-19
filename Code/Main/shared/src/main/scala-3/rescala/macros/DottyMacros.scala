package rescala.macros

import rescala.macros.MacroAccess

import scala.quoted.*

object FindInterp extends ExprMap {
  override def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = {
    import quotes.reflect.*
    e match {
      case '{(${x}: MacroAccess[_, _]).value} =>
        println(x.show)
        '{null.asInstanceOf[T]}
      case _ => transformChildren(e)
    }

  }
}

inline def detectReactives[T](inline expr: T): T =
  ${ detectImpl('expr) }

def detectImpl[T: Type](expr: Expr[T])(using Quotes): Expr[T] =
  println("transforming!")
  val res = FindInterp.transformChildren(expr)
  res
