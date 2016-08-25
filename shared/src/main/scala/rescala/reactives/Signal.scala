package rescala.reactives

import rescala.engines.{Engine, Ticket}
import rescala.graph.Pulse.{Change, Exceptional, NoChange, Stable}
import rescala.graph.{Disconnectable, Pulse, Stateful, Struct}
import rescala.propagation.Turn
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

  final def recover[R >: A](onFailure: Throwable => R)(implicit ticket: Ticket[S]): Signal[R, S] = Signals.static(this) { turn =>
    try this.get(turn) catch {
      case NonFatal(e) => onFailure(e)
    }
  }

  final def withDefault[R >: A](value: R)(implicit ticket: Ticket[S]): Signal[R, S] = Signals.static(this) { (turn) =>
    try this.get(turn) catch {
      case EmptySignalControlThrowable => value
    }
  }

  def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit


  /** Return a Signal with f applied to the value */
  final override def map[B](f: A => B)(implicit ticket: Ticket[S]): Signal[B, S] = Signals.lift(this) {f}

  /** flatten the inner reactive */
  final override def flatten[R](implicit ev: Flatten[A, S, R], ticket: Ticket[S]): R = ev.apply(this)

  /**
    * Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    */
  override def changed(implicit ticket: Ticket[S]): Event[A, S] = Events.static(s"(changed $this)", this) { turn =>
    pulse(turn) match {
      case Pulse.empty => Pulse.NoChange
      case other => other
    }
  }

  /** Create an event that fires every time the signal changes. It fires the tuple (oldVal, newVal) for the signal.
    * Be aware that no change will be triggered when the signal changes to or from empty */
  final override def change(implicit ticket: Ticket[S]) = {
    Events.static(s"(change $this)", this) { turn =>
      val from = stable(turn)
      val to = pulse(turn)
      if (from != to) Pulse.Change(Signals.Diff(from, to))
      else Pulse.NoChange
    }
  }


  final def delay(n: Int)(implicit ticket: Ticket[S]): Signal[A, S] = ticket { implicit turn => changed.delay(this.get, n) }
}
