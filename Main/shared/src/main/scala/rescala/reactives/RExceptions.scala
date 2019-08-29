package rescala.reactives

import rescala.core.ReSource

import scala.util.control.ControlThrowable

object RExceptions {
  object EmptySignalControlThrowable extends ControlThrowable
  class UnhandledFailureException(location: ReSource[_], cause: Throwable) extends RuntimeException(s"a failure was observed but not handled by: $location", cause)
  def toExternalException[R](r: Any, f: => R) = {
    try { f }
    catch {
      case EmptySignalControlThrowable => throw new NoSuchElementException(s"$r is empty")
      case other: Throwable => throw new IllegalStateException(s"$r has an error value", other)
    }
  }
}
