package rescala.reactives

import scala.util.control.ControlThrowable

object RExceptions {
  object EmptySignalControlThrowable extends ControlThrowable
  class UnhandledFailureException(cause: Throwable) extends RuntimeException(cause)
}
