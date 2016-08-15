package rescala.reactives

import rescala.engines.Ticket
import rescala.graph.Pulse.{Change, Exceptional, NoChange, Stable}
import rescala.graph.{Pulse, Stateful, Struct}
import rescala.reactives.RExceptions.{EmptySignalControlThrowable, UnhandledFailureException}
import rescala.reactives.Signals.Flatten

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

  final def withDefault[R >: A](onEmpty: () => R)(implicit ticket: Ticket[S]): Signal[R, S] = Signals.static(this) { (turn) =>
    try this.get(turn) catch {
      case e: EmptySignalControlThrowable => onEmpty()
    }
  }


  /** Return a Signal with f applied to the value */
  final override def map[B](f: A => B)(implicit ticket: Ticket[S]) = Signals.lift(this) {f}

  /** flatten the inner reactive */
  final override def flatten[R](implicit ev: Flatten[A, S ,R], ticket: Ticket[S]): R = ev.apply(this)

  /**
    * Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    */
  override def changed(implicit ticket: Ticket[S]): Event[A, S] = Events.static(s"(changed $this)", this) { turn => pulse(turn) }

  /**
    * Create an event that fires every time the signal changes. It fires the tuple
    * (oldVal, newVal) for the signal. The first tuple is (null, newVal)
    */
  final override def change(implicit ticket: Ticket[S]) = {
    Events.static(s"(change $this)", this) { turn =>
      pulse(turn) match {
        case Change(value) => stable(turn) match {
          case Stable(oldValue) => Pulse.Change((oldValue, value))
          case ex@Exceptional(_) => ex
          case _ => throw new IllegalStateException("Can not compute change from empty signal")
        }
        case NoChange | Stable(_) => Pulse.NoChange
        case ex@Exceptional(t) => ex
      }
    }
  }


  final def delay(n: Int)(implicit ticket: Ticket[S]): Signal[A, S] = ticket { implicit turn => changed.delay(this.get, n) }
}
