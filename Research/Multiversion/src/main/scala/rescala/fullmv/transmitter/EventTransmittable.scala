package rescala.fullmv.transmitter

import rescala.core.{Engine, Pulse, Struct}
import rescala.reactives.Event
import retier.transmitter.{Endpoint, PushBasedTransmittable, RemoteRef, Transmittable, Serializable}

import scala.language.higherKinds

object EventTransmittable {
  implicit def rescalaEventTransmittable
      [Evt[T, ES <: Struct] <: Event[T, ES], T, ES <: Struct, S, U](implicit
      engine: Engine[ES],
      transmittable: Transmittable[(T, String), S, (U, String)],
      serializable: Serializable[S]) = {
    type From = (T, String)
    type To = (U, String)

    new PushBasedTransmittable[Evt[T, ES], From, S, To, engine.Event[U]] {
      final val ignoredValue = null.asInstanceOf[T]
      final val ignoredString = null.asInstanceOf[String]

      def send(value: Evt[T, ES], remote: RemoteRef, endpoint: Endpoint[From, To]) = {
        val observer =
          (value
            map { (_, ignoredString) }
            recover { case throwable => Some((ignoredValue, throwable.toString)) }
            observe endpoint.send)

        endpoint.closed notify { _ => observer.remove }

        null
      }

      def receive(value: To, remote: RemoteRef, endpoint: Endpoint[From, To]) = {
        val event = engine.Evt[U]

        endpoint.receive notify {
          _ match {
            case (value, `ignoredString`) =>
              event fire value
            case (_, message) =>
              engine.transaction(event) { implicit turn =>
                event admitPulse Pulse.Exceptional(
                  new RemoteReactiveFailure(message))
              }
          }
        }

        endpoint.closed notify { _ => event.disconnect }

        event
      }
    }
  }
}
