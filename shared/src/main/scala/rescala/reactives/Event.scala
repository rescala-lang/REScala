package rescala.reactives

import rescala.engines.Ticket
import rescala.graph._

import scala.collection.immutable.{Queue, LinearSeq}
import scala.language.higherKinds

/**
  * Base signal interface for all signal implementations.
  * Please note that any event implementation should have the EV type parameter set to itself and be paired with
  * exactly one signal implementation it is compatible with by setting the SL type parameter.
  * This relationship needs to be symmetrical.
  *
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  * @tparam SL Signal type supported as parameter and used as return type for event methods
  * @tparam EV Event type supported as parameter and used as return type for event methods
  */
trait Event[+T, S <: Struct, SL[+X, Z <: Struct] <: Signal[X, Z, SL, EV], EV[+X, Z <: Struct] <: Event[X, Z, SL, EV]] {
  this : EV[T, S] =>

  /** add an observer */
  final def +=(react: T => Unit)(implicit ticket: Ticket[S]): Observe[S] = observe(react)(ticket)
  def observe(react: T => Unit)(implicit ticket: Ticket[S]): Observe[S]

  /**
   * Events disjunction.
   */
  def ||[U >: T](other: EV[U, S])(implicit ticket: Ticket[S]) : EV[U, S]

  /**
   * Event filtered with a predicate
   */
  def &&(pred: T => Boolean)(implicit ticket: Ticket[S]): EV[T, S]
  final def filter(pred: T => Boolean)(implicit ticket: Ticket[S]): EV[T, S] = &&(pred)

  /**
   * Event is triggered except if the other one is triggered
   */
  def \[U](other: EV[U, S])(implicit ticket: Ticket[S]): EV[T, S]

  /**
   * Events conjunction
   */
  def and[U, R](other: EV[U, S])(merger: (T, U) => R)(implicit ticket: Ticket[S]): EV[R, S]

  /**
   * Event conjunction with a merge method creating a tuple of both event parameters
   */
  def zip[U](other: EV[U, S])(implicit ticket: Ticket[S]): EV[(T, U), S]

  /**
   * Transform the event parameter
   */
  def map[U](mapping: T => U)(implicit ticket: Ticket[S]): EV[U, S]

  /**
   * Drop the event parameter; equivalent to map((_: Any) => ())
   */
  final def dropParam(implicit ticket: Ticket[S]): EV[Unit, S] = map(_ => ())


  /** folds events with a given fold function to create a Signal */
  def fold[A](init: A)(fold: (A, T) => A)(implicit ticket: Ticket[S]): SL[A, S]

  /** Iterates a value on the occurrence of the event. */
  final def iterate[A](init: A)(f: A => A)(implicit ticket: Ticket[S]): SL[A, S] = fold(init)((acc, _) => f(acc))

  /**
   * Counts the occurrences of the event. Starts from 0, when the event has never been
   * fired yet. The argument of the event is simply discarded.
   */
  final def count()(implicit ticket: Ticket[S]): SL[Int, S] = fold(0)((acc, _) => acc + 1)

  /**
   * Calls f on each occurrence of event e, setting the Signal to the generated value.
   * The initial signal is obtained by f(init)
   */
  final def set[B >: T, A](init: B)(f: (B => A))(implicit ticket: Ticket[S]): SL[A, S] = fold(f(init))((_, v) => f(v))

  /** returns a signal holding the latest value of the event. */
  final def latest[T1 >: T](init: T1)(implicit ticket: Ticket[S]): SL[T1, S] = fold(init)((_, v) => v)

  /** Holds the latest value of an event as an Option, None before the first event occured */
  final def latestOption()(implicit ticket: Ticket[S]): SL[Option[T], S] = fold(None: Option[T]) { (_, v) => Some(v) }

  /** calls factory on each occurrence of event e, resetting the Signal to a newly generated one */
  final def reset[T1 >: T, A](init: T1)(factory: T1 => SL[A, S])(implicit ticket: Ticket[S]): SL[A, S] = set(init)(factory).flatten()

  /**
   * Returns a signal which holds the last n events in a list. At the beginning the
   * list increases in size up to when n values are available
   */
  final def last(n: Int)(implicit ticket: Ticket[S]): SL[LinearSeq[T], S] =
    fold(Queue[T]()) { (queue: Queue[T], v: T) =>
      if (queue.length >= n) queue.tail.enqueue(v) else queue.enqueue(v)
    }

  /** collects events resulting in a variable holding a list of all values. */
  final def list()(implicit ticket: Ticket[S]): SL[List[T], S] = fold(List[T]())((acc, v) => v :: acc)

  /** Switch back and forth between two signals on occurrence of event e */
  def toggle[A](a: SL[A, S], b: SL[A, S])(implicit ticket: Ticket[S]): SL[A, S]

  /** Return a Signal that is updated only when e fires, and has the value of the signal s */
  def snapshot[A](s: SL[A, S])(implicit ticket: Ticket[S]): SL[A, S]

  /** Switch to a new Signal once, on the occurrence of event e. */
  def switchOnce[A](original: SL[A, S], newSignal: SL[A, S])(implicit ticket: Ticket[S]): SL[A, S]

  /**
   * Switch to a signal once, on the occurrence of event e. Initially the
   * return value is set to the original signal. When the event fires,
   * the result is a constant signal whose value is the value of the event.
   */
  def switchTo[T1 >: T](original: SL[T1, S])(implicit ticket: Ticket[S]): SL[T1, S]

  /** Like latest, but delays the value of the resulting signal by n occurrences */
  final def delay[T1 >: T](init: T1, n: Int)(implicit ticket: Ticket[S]): SL[T1, S] = {
    val history: SL[LinearSeq[T], S] = last(n + 1)
    history.map { h => if (h.size <= n) init else h.head }
  }

  /** returns the values produced by the last event produced by mapping this value */
  def flatMap[B](f: T => EV[B, S])(implicit ticket: Ticket[S]): EV[B, S]

  /** promotes the latest inner event to an outer event */
  final def flatten[B]()(implicit ticket: Ticket[S], ev: T <:< EV[B, S]): EV[B, S] = flatMap(ev.apply)

  /** logs the events to a signal */
  final def log()(implicit ticket: Ticket[S]): SL[List[T], S] = fold[List[T]](Nil)((a, v) => v :: a)
}
