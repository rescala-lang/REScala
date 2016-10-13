package rescala.reactives

import rescala.graph.Reactive

import scala.util.control.ControlThrowable

object RExceptions {
  object EmptySignalControlThrowable extends ControlThrowable
  class UnhandledFailureException(location: Reactive[_], cause: Throwable) extends RuntimeException(s"a failure was observed but not handled by: $location", cause)
}
