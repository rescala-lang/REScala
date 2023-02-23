package rescala.operator

import rescala.core.{CreationTicket, ReadAs}

import scala.annotation.implicitNotFound
import scala.collection.IterableOps
import scala.reflect.ClassTag

trait FlattenApi {
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

  /** Flatten a Signal[Traversable[Signal[B]\]\] into a Signal[Traversable[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  implicit def flattenImplicitFortraversableSignals[B, T[U] <: IterableOps[U, T, T[U]], Sig[A1] <: Signal[A1]](implicit
      ticket: CreationTicket[State]
  ): Flatten[Signal[T[Sig[B]]], Signal[T[B]]] =
    new Flatten[Signal[T[Sig[B]]], Signal[T[B]]] {
      def apply(sig: Signal[T[Sig[B]]]): Signal[T[B]] =
        Signals.dynamic(sig) { t => t.depend(sig).map { (r: Signal[B]) => t.depend(r) } }
    }

  /** Flatten a Signal[Traversable[Event[B]\]\] into a Event[B]. The new Event fires the value of any inner firing Event.
    * If multiple inner Events fire, the first one in iteration order is selected.
    */
  def firstFiringEvent[B, T[U] <: IterableOps[U, T, T[U]], Evnt[A1] <: ReadAs.of[State, Option[A1]]](
      implicit ticket: CreationTicket[State]
  ): Flatten[Signal[T[Evnt[B]]], Event[B]] =
    new Flatten[Signal[T[Evnt[B]]], Event[B]] {
      def apply(sig: Signal[T[Evnt[B]]]): Event[B] =
        Events.dynamic(sig) { t =>
          val all = t.depend(sig) map { (r: ReadAs.of[State, Option[B]]) => t.depend[Option[B]](r) }
          all.collectFirst { case Some(e) => e }
        }
    }

  /** Flatten a Signal[Traversable[Event[B]\]\] into a Event[Traversable[Option[B]\]\] where the new Event fires whenever any of the inner events fire */
  def traversableOfAllOccuringEventValues[B, T[U] <: IterableOps[U, T, T[U]], Evnt[A1] <: Event[A1]](implicit
      ticket: CreationTicket[State]
  ): Flatten[Signal[T[Evnt[B]]], Event[T[Option[B]]]] =
    new Flatten[Signal[T[Evnt[B]]], Event[T[Option[B]]]] {
      def apply(sig: Signal[T[Evnt[B]]]): Event[T[Option[B]]] =
        Events.dynamic(sig) { t =>
          val all = t.depend(sig) map { (r: Event[B]) => t.depend(r) }
          if (all.exists(_.isDefined)) Some(all) else None
        }
    }
}
