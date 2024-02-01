package loci
package transmitter

import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Success, Try}

@implicitNotFound("${B} is not marshallable")
trait Marshallable[B, R, P] {
  def marshal(value: B, abstraction: AbstractionRef): MessageBuffer
  def unmarshal(value: MessageBuffer, abstraction: AbstractionRef): Try[R]
  def unmarshal(value: Notice.Steady[Try[MessageBuffer]], abstraction: AbstractionRef): P

  type Type = Marshallable[B, R, P]

  @inline final def self: Type = this

  def connected: Boolean
}

sealed trait MarshallableResolution {
  @inline def apply[T](implicit marshallable: Marshallable[T, _, _])
  : marshallable.Type = marshallable.self

  @inline def Argument[T](implicit marshallable: Marshallable[T, T, _])
  : marshallable.Type = marshallable.self

  implicit def marshallable[B, I, R, P, T <: Transmittables](implicit
      resolution: Transmittable.Resolution[B, I, R, P, T],
      serializer: Serializable[I],
      contextBuilder: ContextBuilder[T]): Marshallable[B, R, P] =
    new Marshallable[B, R, P] {
      val transmittable = resolution.transmittable

      def connected = (transmittable: Transmittable.Any[B, I, R]) match {
        case _: ConnectedTransmittable[_, _, _] => true
        case _: ConnectedTransmittable.Proxy[_, _, _] => true
        case _ => false
      }

      def marshal(value: B, abstraction: AbstractionRef) =
        try {
          implicit val context = contextBuilder(
            transmittable.transmittables, abstraction, ContextBuilder.sending)
          serializer serialize (transmittable buildIntermediate value)
        }
        catch {
          case NonFatal(exception) =>
            throw new RemoteAccessException(s"marshalling failed: $value").initCause(exception)
        }

      def unmarshal(value: MessageBuffer, abstraction: AbstractionRef) =
        try {
          implicit val context = contextBuilder(
            transmittable.transmittables, abstraction, ContextBuilder.receiving)
          serializer deserialize value map transmittable.buildResult
        }
        catch {
          case NonFatal(exception) =>
            throw new RemoteAccessException(s"unmarshalling failed: $value").initCause(exception)
        }

      def unmarshal(value: Notice.Steady[Try[MessageBuffer]], abstraction: AbstractionRef) =
        try {
          implicit val context = contextBuilder(
            transmittable.transmittables, abstraction, ContextBuilder.receiving)
          transmittable buildProxy (
            value map { _ flatMap serializer.deserialize })
        }
        catch {
          case NonFatal(exception) =>
            throw new RemoteAccessException("unmarshalling failed: could not create proxy object").initCause(exception)
        }
    }
}

object Marshallable extends MarshallableResolution {
  implicit object unit extends Marshallable[Unit, Unit, Future[Unit]] {
    def marshal(value: Unit, abstraction: AbstractionRef) =
      MessageBuffer.empty
    def unmarshal(value: MessageBuffer, abstraction: AbstractionRef) =
      Success(())
    def unmarshal(value: Notice.Steady[Try[MessageBuffer]], abstraction: AbstractionRef) =
      (value map { _ map { _ => () } }).toFutureFromTry
    def connected = false
  }

  implicit object nothing extends Marshallable[Nothing, Nothing, Future[Nothing]] {
    def nothing = throw new RemoteAccessException("Unexpected value of bottom type")
    def marshal(value: Nothing, abstraction: AbstractionRef) =
      nothing
    def unmarshal(value: MessageBuffer, abstraction: AbstractionRef) =
      nothing
    def unmarshal(value: Notice.Steady[Try[MessageBuffer]], abstraction: AbstractionRef) =
      (value map { _ map { _ => nothing } }).toFutureFromTry
    def connected = false
  }
}
