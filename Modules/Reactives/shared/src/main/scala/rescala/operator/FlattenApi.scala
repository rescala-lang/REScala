package rescala.operator

import rescala.compat.FlattenCollectionCompat
import rescala.core.CreationTicket

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

trait FlattenApi extends FlattenCollectionCompat {
  self: Operators =>
  @implicitNotFound(msg =
    "Could not flatten ${A}. Try to select a specific flatten strategy from rescala.reactives.Flatten."
  )
  trait Flatten[-A, R] {
    def apply(sig: A): R
  }

  /** Flatten a Signal[Signal[B]\] into a Signal[B] that changes whenever the outer or inner signal changes. */
  implicit def flattenImplicitForsignal[B](implicit
      ticket: CreationTicket[State]
  ): Flatten[Signal[Signal[B]], Signal[B]] =
    new Flatten[Signal[Signal[B]], Signal[B]] {
      def apply(sig: Signal[Signal[B]]): Signal[B] =
        Signals.dynamic(sig) { t => t.depend(t.depend(sig).resource) }
    }

  /** Flatten a Signal[Array[Signal[B]\]\] into a Signal[Array[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  implicit def flattenImplicitForarraySignals[B: ClassTag, Sig[U] <: Signal[U]](implicit
      ticket: CreationTicket[State]
  ): Flatten[Signal[Array[Sig[B]]], Signal[Array[B]]] =
    new Flatten[Signal[Array[Sig[B]]], Signal[Array[B]]] {
      def apply(sig: Signal[Array[Sig[B]]]): Signal[Array[B]] =
        Signals.dynamic(sig) { t => t.depend(sig) map { (r: Signal[B]) => t.depend(r) } }
    }

  /** Flatten a Signal[Option[Signal[B]\]\] into a Signal[Option[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  implicit def flattenImplicitForoptionSignal[B, Sig[U] <: Signal[U]](implicit
      ticket: CreationTicket[State]
  ): Flatten[Signal[Option[Sig[B]]], Signal[Option[B]]] =
    new Flatten[Signal[Option[Sig[B]]], Signal[Option[B]]] {
      def apply(sig: Signal[Option[Sig[B]]]): Signal[Option[B]] =
        Signals.dynamic(sig) { t => t.depend(sig) map { (r: Signal[B]) => t.depend(r) } }
    }

  /** Flatten a Signal[Event[B]]\] into a Event[B] where the new Event fires whenever the current inner event fires */
  implicit def flattenImplicitForevent[A, B, Evnt[A1] <: Event[A1]](implicit
      ticket: CreationTicket[State]
  ): Flatten[Signal[Evnt[B]], Event[B]] =
    new Flatten[Signal[Evnt[B]], Event[B]] {
      def apply(sig: Signal[Evnt[B]]): Event[B] = Events.dynamic(sig) { t => t.depend(t.depend(sig)) }
    }

  /** Flatten a Event[Option[B]\] into a Event[B] that fires whenever the inner option is defined. */
  implicit def flattenImplicitForoption[A, B](implicit
      ticket: CreationTicket[State]
  ): Flatten[Event[Option[B]], Event[B]] =
    new Flatten[Event[Option[B]], Event[B]] {
      def apply(event: Event[Option[B]]): Event[B] =
        Events.static(event) { t => t.dependStatic(event).flatten }
    }

}
