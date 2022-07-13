package rescala.api2

import scala.quoted.*

inline def printCode[T](inline expr: T): Unit =
  ${ impl('expr) }

def impl[T: Type](expr: Expr[T])(using quotes: Quotes): Expr[Unit] = {

  import quotes.reflect.*
  val e = expr.show.toString
  val t = expr.asTerm.toString
  println(e)
  println(t)
  '{
  println(${Expr(e)})
  //println(${Expr(t.length)})
  }

}

