package rescala.reactives

import scala.util.{Failure, Success, Try}
import scala.util.control.{ControlThrowable, NonFatal}

object RExceptions {
  object EmptySignalControlThrowable extends ControlThrowable
  class UnhandledFailureException(cause: Throwable) extends RuntimeException(cause)

  def reTry[T](f: => T): Try[T] = try Success(f) catch {
    case EmptySignalControlThrowable => Failure(EmptySignalControlThrowable)
    case NonFatal(t) => Failure(t)
  }
}
