package rescala.reactives

import java.util.concurrent.CompletionException

import rescala.engines.Ticket
import rescala.graph.Pulse.{Diff, Exceptional, NoChange}
import rescala.graph.{Stateful, Struct}

import scala.util.{Failure, Success, Try}

trait Signal[+A, S <: Struct] extends Stateful[A, S] {

  /** add an observer */
  final def observe(react: A => Unit)(implicit ticket: Ticket[S]): Observe[S] = Observe(this){
    case Success(v) => react(v)
    case Failure(t) => throw new CompletionException("Unhandled exception on observe", t)
  }

  final def toTry()(implicit ticket: Ticket[S]): Signal[Try[A], S] = Signals.static(this){ turn =>
    this.pulse(turn).toOptionTry().getOrElse(throw new IllegalStateException("reevaluation without changes"))
  }

  /** Return a Signal with f applied to the value */
  final def map[B](f: A => B)(implicit ticket: Ticket[S]): Signal[B, S] = Signals.lift(this) {f}

  /** flatten the inner signal */
  final def flatten[B]()(implicit ev: A <:< Signal[B, S], ticket: Ticket[S]) = Signals.dynamic(this) { s => this (s)(s) }

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
  final def unwrap[E](implicit evidence: A <:< Event[E, S], ticket: Ticket[S]): Event[E, S] = Events.wrapped(map(evidence))

  /**
    * Create an event that fires every time the signal changes. It fires the tuple
    * (oldVal, newVal) for the signal. The first tuple is (null, newVal)
    */
  final def change(implicit ticket: Ticket[S]): Event[(A, A), S] = Events.change(this)

  /**
    * Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    */
  final def changed(implicit ticket: Ticket[S]): Event[A, S] = change.map(_._2)

  /** Convenience function filtering to events which change this reactive to value */
  final def changedTo[V](value: V)(implicit ticket: Ticket[S]): Event[Unit, S] = (changed && {_ == value}).dropParam

}
