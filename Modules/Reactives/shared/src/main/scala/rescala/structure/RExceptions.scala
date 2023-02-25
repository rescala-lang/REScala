package rescala.structure

import scala.util.control.ControlThrowable

object RExceptions {
  object EmptySignalControlThrowable extends ControlThrowable

  case class ObservedException(location: Any, details: String, cause: Throwable) extends RuntimeException(cause) {
    override def getMessage: String = {
      val nestedMessage = Option(cause.getMessage).fold("") { msg => s" $msg" }
      s"»$location« $details: ${cause}$nestedMessage"
    }
  }

  def toExternalReadException[R](r: Any, f: => R): R = {
    try { f }
    catch {
      case EmptySignalControlThrowable => throw new NoSuchElementException(s"$r is empty")
      // todo improve error message
      case other: Throwable => throw ObservedException(r, "was accessd but contained an exception", other)
    }
  }

}
