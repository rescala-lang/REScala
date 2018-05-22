package rescala.reactives

import rescala.core.{CreationTicket, Struct}

import scala.annotation.implicitNotFound
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.reflect.ClassTag

@implicitNotFound(msg = "Could not flatten ${A}. Try to select a specific flatten strategy from rescala.reactives.Flatten.")
sealed trait Flatten[-A, R] {
  def apply(sig: A): R
}
object Flatten {
  /** Flatten a Signal[Signal[B]\] into a Signal[B] that changes whenever the outer or inner signal changes. */
  implicit def signal
  [S <: Struct, B, Sig[U, V <: Struct] <: Signal[U, V]]
    (implicit ticket: CreationTicket[S]): Flatten[Signal[Sig[B, S], S], Signal[B, S]] = new Flatten[Signal[Sig[B, S], S], Signal[B, S]] {
    def apply(sig: Signal[Sig[B, S], S]): Signal[B, S] = Signals.dynamic(sig) { t => t.depend(t.depend(sig)) }
  }
  /** Flatten a Signal[Traversable[Signal[B]\]\] into a Signal[Traversable[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  implicit def traversableSignals
  [S <: Struct, B, T[U] <: TraversableLike[U, T[U]], Sig[A1, S1 <: Struct] <: Signal[A1, S1]]
    (implicit ticket: CreationTicket[S], cbf: CanBuildFrom[T[_], B, T[B]]): Flatten[Signal[T[Sig[B, S]], S], Signal[T[B], S]] = new Flatten[Signal[T[Sig[B, S]], S], Signal[T[B], S]] {
    def apply(sig: Signal[T[Sig[B, S]], S]): Signal[T[B], S] = Signals.dynamic(sig) { t => t.depend(sig) map { r: Signal[B, S] => t.depend(r) } }
  }

  /** Flatten a Signal[Array[Signal[B]\]\] into a Signal[Array[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  implicit def arraySignals
  [S <: Struct, B: ClassTag, Sig[U, V <: Struct] <: Signal[U, V]](implicit ticket: CreationTicket[S])
  : Flatten[Signal[Array[Sig[B, S]], S], Signal[Array[B], S]] = new Flatten[Signal[Array[Sig[B, S]], S], Signal[Array[B], S]] {
    def apply(sig: Signal[Array[Sig[B, S]], S]): Signal[Array[B], S] = Signals.dynamic(sig) { t => t.depend(sig) map { r: Signal[B, S] => t.depend(r) } }
  }

  /** Flatten a Signal[Option[Signal[B]\]\] into a Signal[Option[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  implicit def optionSignal
  [S <: Struct, B, Sig[U, V <: Struct] <: Signal[U, V]](implicit ticket: CreationTicket[S])
  : Flatten[Signal[Option[Sig[B, S]], S], Signal[Option[B], S]] = new Flatten[Signal[Option[Sig[B, S]], S], Signal[Option[B], S]] {
    def apply(sig: Signal[Option[Sig[B, S]], S]): Signal[Option[B], S] = Signals.dynamic(sig) { t => t.depend(sig) map { r: Signal[B, S] => t.depend(r) } }
  }

  /** Flatten a Signal[Event[B]]\] into a Event[B] where the new Event fires whenever the current inner event fires */
  implicit def event
  [A, S <: Struct, B, Evnt[A1, S1 <: Struct] <: Event[A1, S1]]
    (implicit ticket: CreationTicket[S]): Flatten[Signal[Evnt[B, S], S], Event[B, S]] = new Flatten[Signal[Evnt[B, S], S], Event[B, S]] {
    def apply(sig: Signal[Evnt[B, S], S]): Event[B, S] = Events.dynamic(sig) { t => t.depend(t.depend(sig)) }
  }

  /** Flatten a Signal[Traversable[Event[B]\]\] into a Event[B]. The new Event fires the value of any inner firing Event.
    * If multiple inner Events fire, the first one in iteration order is selected. */
  def firstFiringEvent
  [S <: Struct, B, T[U] <: TraversableLike[U, T[U]], Evnt[A1, S1 <: Struct] <: Event[A1, S1]]
    (implicit ticket: CreationTicket[S], cbf: CanBuildFrom[T[_], Option[B], T[Option[B]]]): Flatten[Signal[T[Evnt[B, S]], S], Event[B, S]] = new Flatten[Signal[T[Evnt[B, S]], S], Event[B, S]] {
    def apply(sig: Signal[T[Evnt[B, S]], S]): Event[B, S] = Events.dynamic(sig) { t =>
      val all = t.depend(sig) map { r: Event[B, S] => t.depend(r) }
      all.collectFirst{ case Some(e) => e }
    }
  }

  /** Flatten a Signal[Traversable[Event[B]\]\] into a Event[Traversable[Option[B]\]\] where the new Event fires whenever any of the inner events fire */
  def traversableOfAllOccuringEventValues
  [S <: Struct, B, T[U] <: TraversableLike[U, T[U]], Evnt[A1, S1 <: Struct] <: Event[A1, S1]]
    (implicit ticket: CreationTicket[S], cbf: CanBuildFrom[T[_], Option[B], T[Option[B]]]): Flatten[Signal[T[Evnt[B, S]], S], Event[T[Option[B]], S]] = new Flatten[Signal[T[Evnt[B, S]], S], Event[T[Option[B]], S]] {
    def apply(sig: Signal[T[Evnt[B, S]], S]): Event[T[Option[B]], S] = Events.dynamic(sig) { t =>
      val all = t.depend(sig) map { r: Event[B, S] => t.depend(r) }
      if (all.exists(_.isDefined)) Some(all) else None
    }
  }

  /** Flatten a Event[Option[B]\] into a Event[B] that fires whenever the inner option is defined. */
  implicit def option
  [A, S <: Struct, B, Evnt[A1, S1 <: Struct] <: Event[A1, S1]]
    (implicit ticket: CreationTicket[S]): Flatten[Event[Option[B], S], Event[B, S]] = new Flatten[Event[Option[B], S], Event[B, S]] {
    def apply(event: Event[Option[B], S]): Event[B, S] = Events.static(event) { t => t.dependStatic(event).flatten }
  }
}
