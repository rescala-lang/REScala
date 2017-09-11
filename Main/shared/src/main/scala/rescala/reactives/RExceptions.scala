package rescala.reactives

import rescala.core.ReSource

import scala.util.control.ControlThrowable

object RExceptions {
  object EmptySignalControlThrowable extends ControlThrowable
  class UnhandledFailureException(location: ReSource[_], cause: Throwable) extends RuntimeException(s"a failure was observed but not handled by: $location", cause)
}
