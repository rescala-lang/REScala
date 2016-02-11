package rescala.util

import scala.language.implicitConversions
import java.util.function.UnaryOperator

object JavaFunctionsImplicits {

  implicit def buildUnaryOp[T](op : T => T) : UnaryOperator[T] = new UnaryOperator[T] {
      override def apply(t : T) = op(t)
   }
  
}