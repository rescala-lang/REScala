package rescala.reactives

import rescala.engines.Ticket
import rescala.graph.{Stateful, StatefulImpl, Struct}

trait Signal[+A, S <: Struct] extends Stateful[A, S] {

  /** add an observer */
  def observe(react: A => Unit)(implicit ticket: Ticket[S]): Observe[S]

  /** Return a Signal with f applied to the value */
  def map[B](f: A => B)(implicit ticket: Ticket[S]): Signal[B, S]

  /** flatten the inner signal */
  def flatten[B]()(implicit ev: A <:< Signal[B, S], ticket: Ticket[S]) : Signal[B, S]

  /** Return a Signal that gets updated only when e fires, and has the value of this Signal */
  final def snapshot(e: Event[_, S])(implicit ticket: Ticket[S]): Signal[A, S] = e.snapshot(this)

  /** Switch to (and keep) event value on occurrence of e */
  final def switchTo[U >: A](e: Event[U, S])(implicit ticket: Ticket[S]): Signal[U, S] = e.switchTo(this)

  /** Switch to (and keep) event value on occurrence of e */
  final def switchOnce[V >: A](e: Event[_, S])(newSignal: Signal[V, S])(implicit ticket: Ticket[S]): Signal[V, S] = e.switchOnce(this, newSignal)

  /** Switch back and forth between this and the other Signal on occurrence of event e */
  final def toggle[V >: A](e: Event[_, S])(other: Signal[V, S])(implicit ticket: Ticket[S]): Signal[V, S] = e.toggle(this, other)

  /** Delays this signal by n occurrences */
  final def delay(n: Int)(implicit ticket: Ticket[S]): Signal[A, S] = ticket { implicit turn => changed.delay(this.get, n) }

  /** Unwraps a Signal[Event[E, S], S] to an Event[E, S] */
  def unwrap[E](implicit evidence: A <:< Event[E, S], ticket: Ticket[S]): Event[E, S]

  /**
    * Create an event that fires every time the signal changes. It fires the tuple
    * (oldVal, newVal) for the signal. The first tuple is (null, newVal)
    */
  def change(implicit ticket: Ticket[S]): Event[(A, A), S]

  /**
    * Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    */
  final def changed(implicit ticket: Ticket[S]): Event[A, S] = change.map(_._2)

  /** Convenience function filtering to events which change this reactive to value */
  final def changedTo[V](value: V)(implicit ticket: Ticket[S]): Event[Unit, S] = (changed && {_ == value}).dropParam

}

trait SignalImpl[+A, S <: Struct] extends Signal[A, S] with StatefulImpl[A, S] {

  /** add an observer */
  final override def observe(react: A => Unit)(implicit ticket: Ticket[S]): Observe[S] = Observe(this)(react)

  /** Return a Signal with f applied to the value */
  final override def map[B](f: A => B)(implicit ticket: Ticket[S]): Signal[B, S] = Signals.lift(this) {f}

  /** flatten the inner signal */
  final override def flatten[B]()(implicit ev: A <:< Signal[B, S], ticket: Ticket[S]) = Signals.dynamic(this) { s => this (s)(s) }

  /** Unwraps a Signal[Event[E, S], S] to an Event[E, S] */
  final override def unwrap[E](implicit evidence: A <:< Event[E, S], ticket: Ticket[S]): Event[E, S] = Events.wrapped(map(evidence))

  /**
    * Create an event that fires every time the signal changes. It fires the tuple
    * (oldVal, newVal) for the signal. The first tuple is (null, newVal)
    */
  final override def change(implicit ticket: Ticket[S]): Event[(A, A), S] = Events.change(this)
}