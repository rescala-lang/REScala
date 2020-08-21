package rescala.reactives

import rescala.core.{CreationTicket, Struct}
import rescala.interface.RescalaInterface

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

@implicitNotFound(msg = "Could not flatten ${A}. Try to select a specific flatten strategy from rescala.reactives.Flatten.")
trait Flatten[-A, R] {
  def apply(sig: A): R
}
object Flatten extends rescala.compat.FlattenCollectionCompat {
  /** Flatten a Signal[Signal[B]\] into a Signal[B] that changes whenever the outer or inner signal changes. */
  implicit def signal
  [S <: Struct, B, Sig[U] <: Signal[U, S]]
  (implicit ticket: CreationTicket[S], api: RescalaInterface[S])
  : Flatten[Signal[Sig[B], S], Signal[B, S]] = new Flatten[Signal[Sig[B], S], Signal[B, S]] {
    def apply(sig: Signal[Sig[B], S]): Signal[B, S] = api.Signals.dynamic(sig) { t => t.depend(t.depend(sig).resource) }
  }


  /** Flatten a Signal[Array[Signal[B]\]\] into a Signal[Array[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  implicit def arraySignals
  [S <: Struct, B: ClassTag, Sig[U] <: Signal[U, S]](implicit ticket: CreationTicket[S], api: RescalaInterface[S])
  : Flatten[Signal[Array[Sig[B]], S], Signal[Array[B], S]] = new Flatten[Signal[Array[Sig[B]], S], Signal[Array[B], S]] {
    def apply(sig: Signal[Array[Sig[B]], S]): Signal[Array[B], S] = api.Signals.dynamic(sig) { t => t.depend(sig) map { r: Signal[B, S] => t.depend(r) } }
  }

  /** Flatten a Signal[Option[Signal[B]\]\] into a Signal[Option[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  implicit def optionSignal
  [S <: Struct, B, Sig[U] <: Signal[U, S]](implicit ticket: CreationTicket[S], api: RescalaInterface[S])
  : Flatten[Signal[Option[Sig[B]], S], Signal[Option[B], S]] = new Flatten[Signal[Option[Sig[B]], S], Signal[Option[B], S]] {
    def apply(sig: Signal[Option[Sig[B]], S]): Signal[Option[B], S] = api.Signals.dynamic(sig) { t => t.depend(sig) map { r: Signal[B, S] => t.depend(r) } }
  }

  /** Flatten a Signal[Event[B]]\] into a Event[B] where the new Event fires whenever the current inner event fires */
  implicit def event
  [A, S <: Struct, B, Evnt[A1, S1 <: Struct] <: Event[A1, S1]]
    (implicit ticket: CreationTicket[S], api: RescalaInterface[S])
  : Flatten[Signal[Evnt[B, S], S], Event[B, S]] = new Flatten[Signal[Evnt[B, S], S], Event[B, S]] {
    def apply(sig: Signal[Evnt[B, S], S]): Event[B, S] = api.Events.dynamic(sig) { t => t.depend(t.depend(sig)) }
  }

  /** Flatten a Event[Option[B]\] into a Event[B] that fires whenever the inner option is defined. */
  implicit def option
  [A, S <: Struct, B]
  (implicit ticket: CreationTicket[S], api: RescalaInterface[S])
  : Flatten[Event[Option[B], S], Event[B, S]] = new Flatten[Event[Option[B], S], Event[B, S]] {
    def apply(event: Event[Option[B], S]): Event[B, S] = api.Events.static(event) { t => t.dependStatic(event).flatten }
  }




}
