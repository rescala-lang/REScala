package rescala.compat

import rescala.operator.Operators
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

trait FlattenCollectionCompat {
  self: Operators =>

  /** Flatten a Signal[Traversable[Signal[B]\]\] into a Signal[Traversable[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  implicit def traversableSignals[B, T[U] <: TraversableLike[U, T[U]], Sig[A1] <: Signal[A1]](implicit
      ticket: CreationTicket,
      cbf: CanBuildFrom[T[_], B, T[B]]
  ): Flatten[Signal[T[Sig[B]]], Signal[T[B]]] =
    new Flatten[Signal[T[Sig[B]]], Signal[T[B]]] {
      def apply(sig: Signal[T[Sig[B]]]): Signal[T[B]] =
        Signals.dynamic(sig) { t => t.depend(sig) map { r: Signal[B] => t.depend(r) } }
    }

  /** Flatten a Signal[Traversable[Event[B]\]\] into a Event[B]. The new Event fires the value of any inner firing Event.
    * If multiple inner Events fire, the first one in iteration order is selected.
    */
  def firstFiringEvent[B, T[U] <: TraversableLike[U, T[U]], Evnt[A1] <: Event[A1]](
      implicit
      ticket: CreationTicket,
      cbf: CanBuildFrom[T[_], Option[B], T[Option[B]]]
  ): Flatten[Signal[T[Evnt[B]]], Event[B]] =
    new Flatten[Signal[T[Evnt[B]]], Event[B]] {
      def apply(sig: Signal[T[Evnt[B]]]): Event[B] =
        Events.dynamic(sig) { t =>
          val all = t.depend(sig) map { r: Event[B] => t.depend(r) }
          all.collectFirst { case Some(e) => e }
        }
    }

  /** Flatten a Signal[Traversable[Event[B]\]\] into a Event[Traversable[Option[B]\]\] where the new Event fires whenever any of the inner events fire */
  def traversableOfAllOccuringEventValues[B, T[U] <: TraversableLike[U, T[U]], Evnt[A1] <: Event[A1]](implicit
      ticket: CreationTicket,
      cbf: CanBuildFrom[T[_], Option[B], T[Option[B]]]
  ): Flatten[Signal[T[Evnt[B]]], Event[T[Option[B]]]] =
    new Flatten[Signal[T[Evnt[B]]], Event[T[Option[B]]]] {
      def apply(sig: Signal[T[Evnt[B]]]): Event[T[Option[B]]] =
        Events.dynamic(sig) { t =>
          val all = t.depend(sig) map { r: Event[B] => t.depend(r) }
          if (all.exists(_.isDefined)) Some(all) else None
        }
    }
}
