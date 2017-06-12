package rescala.reactives

import rescala.engine.{Engine, CreationTicket}
import rescala.graph._
import rescala.reactives.RExceptions.{EmptySignalControlThrowable, UnhandledFailureException}
import rescala.reactives.Signals.Diff

import scala.annotation.compileTimeOnly
import scala.util.control.NonFatal

object Signal {
  @annotation.implicitAmbiguous("Do not use now during propagation. You have a Ticket available, use the accessors defined there.")
  implicit object NowAllowed
  @compileTimeOnly("only for implicit conflicts")
  implicit def nowNotAllowed[S <: Struct](implicit @deprecated("unused", "") ticket: AlwaysTicket[S]): NowAllowed.type = ???
}
/**
  * Base signal interface for all signal implementations.
  * Please note that any signal implementation should have the SL type parameter set to itself and be paired with
  * exactly one event implementation it is compatible with by setting the EV type parameter.
  * This relationship needs to be symmetrical.
  *
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
trait Signal[+A, S <: Struct] extends Pulsing[Pulse[A], S] with Observable[A, S] {

  // only used inside macro and will be replaced there
  @compileTimeOnly("Signal.apply can only be used inside of Signal expressions")
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")

  final def now(implicit engine: Engine[S], @deprecated("unused", "") ev: Signal.NowAllowed.type): A = {
    try { engine.singleNow(this).get }
    catch {
      case EmptySignalControlThrowable => throw new NoSuchElementException(s"Signal $this is empty")
      case other: Throwable => throw new IllegalStateException("Signal has an error value", other)
    }
  }

  final def recover[R >: A](onFailure: PartialFunction[Throwable,R])(implicit ticket: CreationTicket[S]): Signal[R, S] = Signals.static(this) { st =>
    try st.staticDepend(this).get catch {
      case NonFatal(e) => onFailure.applyOrElse[Throwable, R](e, throw _)
    }
  }

  // ================== Derivations ==================

  //final def recover[R >: A](onFailure: Throwable => R)(implicit ticket: TurnSource[S]): Signal[R, S] = recover(PartialFunction(onFailure))

  final def abortOnError()(implicit ticket: CreationTicket[S]): Signal[A, S] = recover{case t => throw new UnhandledFailureException(this, t)}

  final def withDefault[R >: A](value: R)(implicit ticket: CreationTicket[S]): Signal[R, S] = Signals.static(this) { (st) =>
    try st.staticDepend(this).get catch {
      case EmptySignalControlThrowable => value
    }
  }

  def disconnect()(implicit engine: Engine[S]): Unit

  /** Return a Signal with f applied to the value */
  final def map[B](f: A => B)(implicit ticket: CreationTicket[S]): Signal[B, S] = Signals.lift(this)(f)

  /** flatten the inner reactive */
  final def flatten[R](implicit ev: Flatten[A, S, R], ticket: CreationTicket[S]): R = ev.apply(this)(ticket)

  /** Delays this signal by n occurrences */
  final def delay(n: Int)(implicit ticket: CreationTicket[S]): Signal[A, S] = ticket { implicit turn => changed.delay(turn.staticBefore(this).get, n) }

  /** Create an event that fires every time the signal changes. It fires the tuple (oldVal, newVal) for the signal.
    * Be aware that no change will be triggered when the signal changes to or from empty */
  final def change(implicit ticket: CreationTicket[S]): Event[Diff[A], S] = Events.change(this)

  /**
    * Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    */
  final def changed(implicit ticket: CreationTicket[S]): Event[A, S] = Events.static(s"(changed $this)", this) { st =>
    st.staticDepend(this) match {
      case Pulse.empty => Pulse.NoChange
      case other => other
    }
  }

  /** Convenience function filtering to events which change this reactive to value */
  final def changedTo[V](value: V)(implicit ticket: CreationTicket[S]): Event[Unit, S] = (changed filter {_ == value}).dropParam
}

