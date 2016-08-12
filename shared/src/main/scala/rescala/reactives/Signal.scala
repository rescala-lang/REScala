package rescala.reactives

import rescala.engines.Ticket
import rescala.graph.{Pulse, Stateful, Struct}
import rescala.reactives.RExceptions.{EmptySignalControlThrowable, UnhandledFailureException}

import scala.util.control.NonFatal
import scala.util.{Failure, Success}

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
  )(implicit ticket: Ticket[S]): Observe[S] = Observe(this) {
    case Success(v) => onSuccess(v)
    case Failure(t) => onFailure(t)
  }

  final def recoverFailure[R >: A](onFailure: Throwable => R)(implicit ticket: Ticket[S]): Signal[R, S] = Signals.static(this) { turn =>
    try this.get(turn) catch {
      case NonFatal(e) => onFailure(e)
    }
  }

  final def recoverEmpty[R >: A](onEmpty: () => R)(implicit ticket: Ticket[S]): Signal[R, S] = Signals.static(this) { (turn) =>
    try this.get(turn) catch {
      case e: EmptySignalControlThrowable => onEmpty()
    }
  }


  /** Return a Signal with f applied to the value */
  final override def map[B](f: A => B)(implicit ticket: Ticket[S]) = Signals.lift(this) {f}

  /** flatten the inner signal */
  final override def flatten[B]()(implicit ev: A <:< Signal[B, S], ticket: Ticket[S]): Signal[B, S] = Signals.dynamic(this) { s => this (s)(s) }

  /** Unwraps a Signal[Event[EV, S], S] to an Event[EV, S] */
  final override def unwrap[E](implicit evidence: A <:< Event[E, S], ticket: Ticket[S]) = Events.wrapped(map(evidence))

  /**
    * Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    */
  override def changed(implicit ticket: Ticket[S]): Event[A, S] = Events.changed(this)

  /**
    * Create an event that fires every time the signal changes. It fires the tuple
    * (oldVal, newVal) for the signal. The first tuple is (null, newVal)
    */
  final override def change(implicit ticket: Ticket[S]) = Events.change(this)



  final def delay(n: Int)(implicit ticket: Ticket[S]): Signal[A, S] = ticket { implicit turn => changed.delay(this.get, n) }
}
