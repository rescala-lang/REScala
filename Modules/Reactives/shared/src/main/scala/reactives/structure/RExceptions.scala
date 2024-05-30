package reactives.structure

import reactives.core.{ReInfo, ReSource}

object RExceptions {
  case class EmptySignalControlThrowable(info: ReInfo)
      extends Exception(s"$info is empty", null, false, false)

  case class ObservedException(location: Any, details: String, cause: Throwable) extends RuntimeException(cause) {
    override def getMessage: String = {
      val nestedMessage = Option(cause.getMessage).fold("") { msg => s" $msg" }
      s"»$location« $details: ${cause}$nestedMessage"
    }
  }

  def toExternalReadException[R](r: ReSource, f: => R): R = {
    try { f }
    catch {
      case esct: EmptySignalControlThrowable =>
        // TODO: there is a API to add a cause to the exception, but only in JDK 15 …
        throw new java.util.NoSuchElementException(s"${r.info} is empty (propagated from ${esct.info}")
      // todo improve error message
      case other: Throwable => throw ObservedException(r.info, "was accessed but contained an exception", other)
    }
  }

}
