package rescala.reactives

import rescala.engines.{Engine, Ticket}
import rescala.graph.{Observable, Pulse, Stateful, Struct}
import rescala.propagation.Turn
import rescala.reactives.RExceptions.{EmptySignalControlThrowable, UnhandledFailureException}

import scala.language.higherKinds
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * Base signal interface for all signal implementations.
  * Please note that any signal implementation should have the SL type parameter set to itself and be paired with
  * exactly one event implementation it is compatible with by setting the EV type parameter.
  * This relationship needs to be symmetrical.
  *
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
trait Signal[+A, S <: Struct] extends Stateful[A, S] with Observable[A, S] {

  /** add an observer */
  override final def observe(
    onSuccess: A => Unit,
    onFailure: Throwable => Unit = t => throw new UnhandledFailureException(t)
  )(implicit ticket: Ticket[S]): Observe[S] = Observe.strong(this) {
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
  final def map[B](f: A => B)(implicit ticket: Ticket[S]): Signal[B, S] = Signals.lift(this)(f)

  /** flatten the inner reactive */
  final def flatten[R](implicit ev: Flatten[A, S, R], ticket: Ticket[S]): R = ev.apply(this)

  /** Delays this signal by n occurrences */
  final def delay(n: Int)(implicit ticket: Ticket[S]): Signal[A, S] = ticket { implicit turn => changed.delay(this.get, n) }

  /** Create an event that fires every time the signal changes. It fires the tuple (oldVal, newVal) for the signal.
    * Be aware that no change will be triggered when the signal changes to or from empty */
  final def change(implicit ticket: Ticket[S]) = {
    Events.static(s"(change $this)", this) { turn =>
      val from = stable(turn)
      val to = pulse(turn)
      if (from != to) Pulse.Change(Signals.Diff(from, to))
      else Pulse.NoChange
    }
  }

  /**
    * Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    */
  final def changed(implicit ticket: Ticket[S]): Event[A, S] = Events.static(s"(changed $this)", this) { turn =>
    pulse(turn) match {
      case Pulse.empty => Pulse.NoChange
      case other => other
    }
  }

  /** Convenience function filtering to events which change this reactive to value */
  final def changedTo[V](value: V)(implicit ticket: Ticket[S]): Event[Unit, S] = (changed filter {_ == value}).dropParam















}

