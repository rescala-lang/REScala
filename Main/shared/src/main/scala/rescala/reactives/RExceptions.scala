package rescala.reactives

import rescala.core.Node

import scala.util.control.ControlThrowable

object RExceptions {
  object EmptySignalControlThrowable extends ControlThrowable
  class UnhandledFailureException(location: Node[_], cause: Throwable) extends RuntimeException(s"a failure was observed but not handled by: $location", cause)
}
