package reactives.operator

import reactives.SelectedScheduler.State
import reactives.core.CreationTicket

import scala.annotation.{implicitNotFound, nowarn}
import scala.collection.IterableOps
import scala.reflect.ClassTag

@implicitNotFound(msg =
  "Could not flatten ${A}. Try to select a specific flatten strategy from rescala.reactives.Flatten."
)
trait Flatten[-A, R] {
  def apply(sig: A): R
}

object Flatten {

  /** Flatten a Signal[Signal[B]\] into a Signal[B] that changes whenever the outer or inner signal changes. */
  given flattenImplicitForsignal[B](using
      CreationTicket[State]
  ): Flatten[
    Signal[Signal[B]],
    Signal[B]
  ] = sig => Signal.dynamic(sig.value.value)

  /** Flatten a Signal[Array[Signal[B]\]\] into a Signal[Array[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  given flattenImplicitForArraySignals[B: ClassTag, Sig[U] <: Signal[U]](using
      CreationTicket[State]
  ): Flatten[
    Signal[Array[Sig[B]]],
    Signal[Array[B]]
  ] = sig => Signal.dynamic { sig.value.map(_.value) }

  /** Flatten a Signal[Option[Signal[B]\]\] into a Signal[Option[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  given flattenImplicitForOptionSignal[B, Sig[U] <: Signal[U]](using
      CreationTicket[State]
  ): Flatten[
    Signal[Option[Sig[B]]],
    Signal[Option[B]]
  ] = sig => Signal.dynamic { sig.value.map { _.value } }

  /** Flatten a Signal[Event[B]]\] into a Event[B] where the new Event fires whenever the current inner event fires */
  given flattenImplicitForevent[A, B, Evnt[A1] <: Event[A1]](using
      CreationTicket[State]
  ): Flatten[
    Signal[Evnt[B]],
    Event[B]
  ] = sig => Event.dynamic(sig.value.value)

  /** Flatten a Event[Option[B]\] into a Event[B] that fires whenever the inner option is defined. */
  given flattenImplicitForoption[A, B](using
      CreationTicket[State]
  ): Flatten[
    Event[Option[B]],
    Event[B]
  ] = event => Event.static { event.value.flatten }

  /** Flatten a Signal[Traversable[Signal[B]\]\] into a Signal[Traversable[B]\] where the new Signal updates whenever any of the inner or the outer signal updates */
  given flattenImplicitForIterableSignals[
      B,
      Iter[U] <: IterableOps[U, Iter, Iter[U]],
      Sig[A1] <: Signal[A1]
  ](using
      CreationTicket[State]
  ): Flatten[
    Signal[Iter[Sig[B]]],
    Signal[Iter[B]]
  ] = sig => Signal.dynamic { sig.value.map { (r: Signal[B]) => r.value } }

  /** Flatten a Signal[Traversable[Event[B]\]\] into a Event[B]. The new Event fires the value of any inner firing Event.
    * If multiple inner Events fire, the first one in iteration order is selected.
    */
  def firstFiringEvent[
      B,
      T[U] <: IterableOps[U, T, T[U]],
      Evnt[A1] <: Event[A1]
  ](using
      CreationTicket[State]
  ): Flatten[
    Signal[T[Evnt[B]]],
    Event[B]
  ] = sig =>
    Event.dynamic {
      val all = sig.value map { _.value }
      all.collectFirst { case Some(e) => e }
    }

  /** Flatten a Signal[Traversable[Event[B]\]\] into a Event[Traversable[Option[B]\]\] where the new Event fires whenever any of the inner events fire */
  def traversableOfAllOccuringEventValues[
      B,
      T[U] <: IterableOps[U, T, T[U]],
      Evnt[A1] <: Event[A1]
  ](using
      CreationTicket[State]
  ): Flatten[
    Signal[T[Evnt[B]]],
    Event[T[Option[B]]]
  ] = sig =>
    Event.dynamic {
      val all = sig.value map { (r: Event[B]) => r.value }
      if all.exists(_.isDefined) then Some(all) else None
    }
}
