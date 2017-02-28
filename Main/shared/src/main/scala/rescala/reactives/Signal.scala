package rescala.reactives

import rescala.engine.{Engine, TurnSource}
import rescala.graph.{Pulse, Stateful, Struct}
import rescala.propagation.Turn
import rescala.reactives.RExceptions.{EmptySignalControlThrowable, UnhandledFailureException}
import rescala.reactives.Signals.Diff

import scala.language.higherKinds
import scala.util.control.NonFatal

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

  final def recover[R >: A](onFailure: Throwable => R)(implicit ticket: TurnSource[S]): Signal[R, S] = Signals.static(this) { turn =>
    try this.regRead(turn) catch {
      case NonFatal(e) => onFailure(e)
    }
  }

  final def abortOnError()(implicit ticket: TurnSource[S]): Signal[A, S] = recover(t => throw new UnhandledFailureException(this, t))

  final def withDefault[R >: A](value: R)(implicit ticket: TurnSource[S]): Signal[R, S] = Signals.static(this) { (turn) =>
    try this.regRead(turn) catch {
      case EmptySignalControlThrowable => value
    }
  }

  def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit

  /** Return a Signal with f applied to the value */
  final def map[B](f: A => B)(implicit ticket: TurnSource[S]): Signal[B, S] = Signals.lift(this)(f)

  /** flatten the inner reactive */
  final def flatten[R](implicit ev: Flatten[A, S, R], ticket: TurnSource[S]): R = ev.apply(this)

  /** Delays this signal by n occurrences */
  final def delay(n: Int)(implicit ticket: TurnSource[S]): Signal[A, S] = ticket { implicit turn => changed.delay(this.regRead, n) }

  /** Create an event that fires every time the signal changes. It fires the tuple (oldVal, newVal) for the signal.
    * Be aware that no change will be triggered when the signal changes to or from empty */
  final def change(implicit ticket: TurnSource[S]): Event[Diff[A], S] = {
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
  final def changed(implicit ticket: TurnSource[S]): Event[A, S] = Events.static(s"(changed $this)", this) { turn =>
    pulse(turn) match {
      case Pulse.empty => Pulse.NoChange
      case other => other
    }
  }

  /** Convenience function filtering to events which change this reactive to value */
  final def changedTo[V](value: V)(implicit ticket: TurnSource[S]): Event[Unit, S] = (changed filter {_ == value}).dropParam


}

