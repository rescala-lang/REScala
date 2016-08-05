package rescala.reactives

import rescala.engines.Ticket
import rescala.graph.{Stateful, Struct}

import scala.language.higherKinds

trait Signal[+A, S <: Struct, SL[+X, Z <: Struct] <: Signal[X, Z, SL, EV], EV[+X, Z <: Struct] <: Event[X, Z, SL, EV]] extends Stateful[A, S] {

  /** add an observer */
  def observe(react: A => Unit)(implicit ticket: Ticket[S]): Observe[S]

  /** Return a Signal with f applied to the value */
  def map[B](f: A => B)(implicit ticket: Ticket[S]): SL[B, S]

  /** flatten the inner signal */
  def flatten[B]()(implicit ev: A <:< SL[B, S], ticket: Ticket[S]) : SL[B, S]

  /** Return a Signal that gets updated only when e fires, and has the value of this Signal */
  final def snapshot(e: EV[_, S])(implicit ticket: Ticket[S]): SL[A, S] = e.snapshot(this.asInstanceOf[SL[A, S]])

  /** Switch to (and keep) event value on occurrence of e */
  final def switchTo[U >: A](e: EV[U, S])(implicit ticket: Ticket[S]): SL[U, S] = e.switchTo(this.asInstanceOf[SL[A, S]])

  /** Switch to (and keep) event value on occurrence of e */
  final def switchOnce[V >: A](e: EV[_, S])(newSignal: SL[V, S])(implicit ticket: Ticket[S]): SL[V, S] = e.switchOnce(this.asInstanceOf[SL[A, S]], newSignal)

  /** Switch back and forth between this and the other Signal on occurrence of event e */
  final def toggle[V >: A](e: EV[_, S])(other: SL[V, S])(implicit ticket: Ticket[S]): SL[V, S] = e.toggle(this.asInstanceOf[SL[A, S]], other)

  /** Delays this signal by n occurrences */
  final def delay(n: Int)(implicit ticket: Ticket[S]): SL[A, S] = ticket { implicit turn => changed.delay(this.get, n) }

  /** Unwraps a Signal[Event[EV, S], S] to an Event[EV, S] */
  def unwrap[E](implicit evidence: A <:< EV[E, S], ticket: Ticket[S]): EV[E, S]

  /**
    * Create an event that fires every time the signal changes. It fires the tuple
    * (oldVal, newVal) for the signal. The first tuple is (null, newVal)
    */
  def change(implicit ticket: Ticket[S]): EV[(A, A), S]

  /**
    * Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    */
  final def changed(implicit ticket: Ticket[S]): EV[A, S] = change.map(_._2)

  /** Convenience function filtering to events which change this reactive to value */
  final def changedTo[V](value: V)(implicit ticket: Ticket[S]): EV[Unit, S] = (changed && {_ == value}).dropParam

}

