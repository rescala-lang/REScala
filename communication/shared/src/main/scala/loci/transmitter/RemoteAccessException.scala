package loci
package transmitter

import Parser._

import scala.util.control.NonFatal

class RemoteAccessException(val reason: RemoteAccessException.Reason) extends RuntimeException(reason.report) {
  def this(message: String) = this(RemoteAccessException.Violation(message))
}

object RemoteAccessException {
  sealed abstract class Reason(val report: String)
  case class RemoteException(name: String, message: String) extends Reason(if (message.nonEmpty) s"$name: $message" else name)
  case class Violation(message: String) extends Reason(message)
  case object RemoteDisconnected extends Reason("remote disconnected")
  case object ChannelClosed extends Reason("communication channel closed")
  case object IllegalSubjectiveAccess extends Reason("illegal subjective access")

  def serialize(exception: Throwable): String =
    serializeException(exception).toString

  private def serializeException(exception: Throwable): Serializer = {
    val RemoteException(name, message) = (Some(exception)
      collect { case exception: RemoteAccessException => exception.reason }
      collect { case exception: RemoteException => exception }
      getOrElse {
        RemoteException(
          exception.getClass.getName,
          if (exception.getMessage == null) "" else exception.getMessage)
      })

    elements(
      string(name),
      string(message),
      list(Option(exception.getCause).toList map serializeException),
      list(exception.getSuppressed.toList map serializeException),
      list(exception.getStackTrace.toList map serializeStackTraceElement))
  }

  private def serializeStackTraceElement(element: StackTraceElement): Serializer =
    elements(
      string(element.getClassName),
      string(element.getMethodName),
      string(element.getFileName),
      string(element.getLineNumber.toString))


  def deserialize(exception: String): RemoteAccessException =
    try deserializeException(parse(exception))
    catch {
      case NonFatal(exception) =>
        val remoteException = new RemoteAccessException("remote exception deserialization failed")
        remoteException.initCause(exception)
        remoteException
    }

  private def deserializeException(exception: Deserializer): RemoteAccessException = {
    val Seq(name, message, cause, suppressed, stackTrace) = exception.asElements(5): @unchecked
    val remoteException = new RemoteAccessException(RemoteException(name.asString, message.asString))
    cause.asList.headOption foreach { cause =>
      remoteException.initCause(deserializeException(cause))
    }
    suppressed.asList foreach { suppressed =>
      remoteException.addSuppressed(deserializeException(suppressed))
    }
    remoteException.setStackTrace((stackTrace.asList map deserializeStackTraceElement).toArray)
    remoteException
  }

  private def deserializeStackTraceElement(element: Deserializer): StackTraceElement = {
    val Seq(className, methodName, fileName, lineNumber) = element.asElements(4): @unchecked
    new StackTraceElement(
      className.asString,
      methodName.asString,
      fileName.asString,
      lineNumber.asString.toInt)
  }
}
