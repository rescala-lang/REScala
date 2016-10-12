package rescala.pipelining.util

import java.util.function.UnaryOperator

import scala.language.implicitConversions

private[pipelining] object JavaFunctionsImplicits {

  implicit def buildUnaryOp[T](op : T => T) : UnaryOperator[T] = new UnaryOperator[T] {
      override def apply(t : T) = op(t)
   }
  
}