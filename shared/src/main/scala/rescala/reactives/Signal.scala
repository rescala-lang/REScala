package rescala.reactives

import java.util.concurrent.CompletionException

import rescala.engines.Ticket
import rescala.graph.{Stateful, Struct}
import rescala.reactives.RExceptions.UnhandledFailureException

import scala.util.{Failure, Success, Try}

/**
  * Standard implementation of the signal interface using Spore-based propagation.
  *
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
trait Signal[+A, S <: Struct] extends SignalLike[A, S, Signal, Event] with Stateful[A, S] {

  /** add an observer */
  final override def observe(
    onSuccess: A => Unit,
    onFailure: Throwable => Unit = t => throw new UnhandledFailureException(t)
  )(implicit ticket: Ticket[S]): Observe[S] = Observe(this){
    case Success(v) => onSuccess(v)
    case Failure(t) => onFailure(t)
  }

  final def toTry()(implicit ticket: Ticket[S]): Signal[Try[A], S] = Signals.static(this){ turn =>
    this.pulse(turn).toOptionTry().getOrElse(throw new IllegalStateException("reevaluation without changes"))
  }


  /** Return a Signal with f applied to the value */
  final override def map[B](f: A => B)(implicit ticket: Ticket[S]) = Signals.lift(this) {f}

  /** flatten the inner signal */
  final override def flatten[B]()(implicit ev: A <:< Signal[B, S], ticket: Ticket[S]) : Signal[B, S] = Signals.dynamic(this) { s => this (s)(s) }

  /** Unwraps a Signal[Event[EV, S], S] to an Event[EV, S] */
  final override def unwrap[E](implicit evidence: A <:< Event[E, S], ticket: Ticket[S]) = Events.wrapped(map(evidence))

  /**
    * Create an event that fires every time the signal changes. It fires the tuple
    * (oldVal, newVal) for the signal. The first tuple is (null, newVal)
    */
  final override def change(implicit ticket: Ticket[S]) = Events.change(this)

  final def delay(n: Int)(implicit ticket: Ticket[S]): Signal[A, S] = ticket { implicit turn => changed.delay(this.get, n) }
}
