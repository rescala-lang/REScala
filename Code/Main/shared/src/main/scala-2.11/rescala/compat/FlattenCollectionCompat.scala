package rescala.compat

import rescala.core.{CreationTicket, Struct}
import rescala.interface.RescalaInterface
import rescala.reactives.{Event, Events, Flatten, Signal, Signals}

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom


trait FlattenCollectionCompat {
  /** Flatten a Signal[Traversable[Signal[B]\]\] into a Signal[Traversable[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  implicit def traversableSignals
  [S <: Struct, B, T[U] <: TraversableLike[U, T[U]], Sig[A1, S1 <: Struct] <: Signal[A1, S1]]
  (implicit ticket: CreationTicket[S], cbf: CanBuildFrom[T[_], B, T[B]], api: RescalaInterface[S])
  : Flatten[Signal[T[Sig[B, S]], S], Signal[T[B], S]] = new Flatten[Signal[T[Sig[B, S]], S], Signal[T[B], S]] {
    def apply(sig: Signal[T[Sig[B, S]], S]): Signal[T[B], S] = api.Signals.dynamic(sig) { t => t.depend(sig) map { r: Signal[B, S] => t.depend(r) } }
  }
  /** Flatten a Signal[Traversable[Event[B]\]\] into a Event[B]. The new Event fires the value of any inner firing Event.
    * If multiple inner Events fire, the first one in iteration order is selected. */
  def firstFiringEvent
  [S <: Struct, B, T[U] <: TraversableLike[U, T[U]], Evnt[A1, S1 <: Struct] <: Event[A1, S1]]
  (implicit ticket: CreationTicket[S], cbf: CanBuildFrom[T[_], Option[B], T[Option[B]]], api: RescalaInterface[S])
  : Flatten[Signal[T[Evnt[B, S]], S], Event[B, S]] = new Flatten[Signal[T[Evnt[B, S]], S], Event[B, S]] {
    def apply(sig: Signal[T[Evnt[B, S]], S]): Event[B, S] = api.Events.dynamic(sig) { t =>
      val all = t.depend(sig) map { r: Event[B, S] => t.depend(r) }
      all.collectFirst{ case Some(e) => e }
    }
  }

  /** Flatten a Signal[Traversable[Event[B]\]\] into a Event[Traversable[Option[B]\]\] where the new Event fires whenever any of the inner events fire */
  def traversableOfAllOccuringEventValues
  [S <: Struct, B, T[U] <: TraversableLike[U, T[U]], Evnt[A1, S1 <: Struct] <: Event[A1, S1]]
  (implicit ticket: CreationTicket[S], cbf: CanBuildFrom[T[_], Option[B], T[Option[B]]], api: RescalaInterface[S])
  : Flatten[Signal[T[Evnt[B, S]], S], Event[T[Option[B]], S]] = new Flatten[Signal[T[Evnt[B, S]], S], Event[T[Option[B]], S]] {
    def apply(sig: Signal[T[Evnt[B, S]], S]): Event[T[Option[B]], S] = api.Events.dynamic(sig) { t =>
      val all = t.depend(sig) map { r: Event[B, S] => t.depend(r) }
      if (all.exists(_.isDefined)) Some(all) else None
    }
  }
}
