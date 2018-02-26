package rescala.reactives

import rescala.core.{Interp, _}
import rescala.reactives.RExceptions.{EmptySignalControlThrowable, UnhandledFailureException}
import rescala.reactives.Signals.{Diff, static}

import scala.util.control.NonFatal

/** Time changing value derived from the dependencies.
  *
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  *
  * @groupname operator Signal operators
  * @groupprio operator 10
  * @groupname conversion Signal to Event conversions
  * @groupprio conversion 20
  * @groupname accessors Accessors and observers
  * @groupprio accessor 5
  */
trait Signal[+A, S <: Struct] extends ReSource[S] with Interp[A, S] with Disconnectable[S] {

  override type Value <: Pulse[A]


  @deprecated("Using now is in most cases not what you want." +
                " It does not build dependencies, does not integrate into transactions." +
                " Use `readValueOnce` for examples and debug output.", "0.23.0")
  final def now(implicit scheduler: Scheduler[S]): A = readValueOnce
  /** Returns the current value of the signal
    * @group accessor */
  final def readValueOnce(implicit scheduler: Scheduler[S]): A = {
    try { scheduler.singleReadValueOnce(this) }
    catch {
      case EmptySignalControlThrowable => throw new NoSuchElementException(s"Signal $this is empty")
      case other: Throwable => throw new IllegalStateException(s"Signal $this has an error value", other)
    }
  }

  /** Interprets the pulse of the signal by returning the value
    * @group internal */
  override def interpret(v: Value): A = v.get

  /** add an observer
    * @group accessor */
  final def observe(
    onSuccess: A => Unit,
    onFailure: Throwable => Unit = null
  )(implicit ticket: CreationTicket[S]): Observe[S] = Observe.strong(this, fireImmediately = true)(onSuccess, onFailure)

  /** Uses a partial function `onFailure` to recover an error carried by the event into a value. */
  final def recover[R >: A](onFailure: PartialFunction[Throwable,R])(implicit ticket: CreationTicket[S]): Signal[R, S] = Signals.static(this) { st =>
    try st.dependStatic(this) catch {
      case NonFatal(e) => onFailure.applyOrElse[Throwable, R](e, throw _)
    }
  }

  // ================== Derivations ==================

  //final def recover[R >: A](onFailure: Throwable => R)(implicit ticket: TurnSource[S]): Signal[R, S] = recover(PartialFunction(onFailure))

  final def abortOnError()(implicit ticket: CreationTicket[S]): Signal[A, S] = recover{case t => throw new UnhandledFailureException(this, t)}

  final def withDefault[R >: A](value: R)(implicit ticket: CreationTicket[S]): Signal[R, S] = Signals.static(this) { (st) =>
    try st.dependStatic(this) catch {
      case EmptySignalControlThrowable => value
    }
  }

  /** Return a Signal with f applied to the value
    * @group operator */
  final def map[B](f: A => B)(implicit ticket: CreationTicket[S]): Signal[B, S] =
    static(this) { t => f(t.dependStatic(this)) }

  /** Flattens the inner reactive.
    * @group operator */
  final def flatten[R](implicit ev: Flatten[A, S, R], ticket: CreationTicket[S]): R = ev.apply(this)(ticket)

//  /** Delays this signal by n occurrences */
//  final def delay[A1 >: A](n: Int)(implicit ticket: CreationTicket[S], ev: ReSerializable[Queue[A1]]): Signal[A1, S] =
//    ticket { implicit ict => changed.delay[A1](ict.turn.staticBefore(this).get, n) }

  /** Create an event that fires every time the signal changes. It fires the tuple (oldVal, newVal) for the signal.
    * Be aware that no change will be triggered when the signal changes to or from empty
    * @group conversion */
  final def change(implicit ticket: CreationTicket[S]): Event[Diff[A], S] = Events.change(this)(ticket)

  /** Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    * @group conversion */
  final def changed(implicit ticket: CreationTicket[S]): Event[A, S] = Events.staticNamed(s"(changed $this)", this) { st =>
    st.collectStatic(this) match {
      case Pulse.empty => Pulse.NoChange
      case other => other
    }
  }

  /** Convenience function filtering to events which change this reactive to value
    * @group conversion */
  final def changedTo[V >: A](value: V)(implicit ticket: CreationTicket[S]): Event[Unit, S] = changed.filter(_ == value).dropParam
}

