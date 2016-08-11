package rescala.reactives

import java.util.concurrent.CompletionException

import rescala.engines.Ticket
import rescala.graph.{StatefulImpl, Struct}

import scala.util.{Failure, Success, Try}

/**
  * Standard implementation of the signal interface using Spore-based propagation.
  *
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
trait SignalImpl[+A, S <: Struct] extends Signal[A, S, SignalImpl, EventImpl] with StatefulImpl[A, S] {

  /** add an observer */
  final override def observe(
    onSuccess: A => Unit,
    onFailure: Throwable => Unit = t => throw new CompletionException("Unhandled exception on observe", t)
  )(implicit ticket: Ticket[S]): Observe[S] = Observe(this){
    case Success(v) => onSuccess(v)
    case Failure(t) => onFailure(t)
  }

  final def toTry()(implicit ticket: Ticket[S]): SignalImpl[Try[A], S] = Signals.static(this){ turn =>
    this.pulse(turn).toOptionTry().getOrElse(throw new IllegalStateException("reevaluation without changes"))
  }


  /** Return a Signal with f applied to the value */
  final override def map[B](f: A => B)(implicit ticket: Ticket[S]) = Signals.lift(this) {f}

  /** flatten the inner signal */
  final override def flatten[B]()(implicit ev: A <:< SignalImpl[B, S], ticket: Ticket[S]) : SignalImpl[B, S] = Signals.dynamic(this) { s => this (s)(s) }

  /** Unwraps a Signal[Event[EV, S], S] to an Event[EV, S] */
  final override def unwrap[E](implicit evidence: A <:< EventImpl[E, S], ticket: Ticket[S]) = Events.wrapped(map(evidence))

  /**
    * Create an event that fires every time the signal changes. It fires the tuple
    * (oldVal, newVal) for the signal. The first tuple is (null, newVal)
    */
  final override def change(implicit ticket: Ticket[S]) = Events.change(this)
}
