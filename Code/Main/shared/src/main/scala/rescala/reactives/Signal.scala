package rescala.reactives

import rescala.core.{Disconnectable, Interp, _}
import rescala.interface.RescalaInterface
import rescala.macros.cutOutOfUserComputation
import rescala.reactives.Observe.ObserveInteract
import rescala.reactives.RExceptions.{EmptySignalControlThrowable, ObservedException}
import rescala.reactives.Signals.Diff

import scala.util.control.NonFatal

/** Time changing value derived from the dependencies.
  *
  * @tparam T Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  *
  * @groupname operator Signal operators
  * @groupprio operator 10
  * @groupname conversion Signal to Event conversions
  * @groupprio conversion 20
  * @groupname accessors Accessors and observers
  * @groupprio accessor 5
  */
object Signal {
  implicit def signalToSignal[T, S <: Struct](signal: Signal[T, S]): rescala.reactives.Signals.SignalResource[T, S] = signal.innerDerived
}
trait Signal[+T, S <: Struct] extends MacroInterp[T, S] with Disconnectable[S] {


  val rescalaAPI: RescalaInterface[S]
  import rescalaAPI.{Signal => Sig, _}


  override def disconnect()(implicit engine: Scheduler[S]): Unit = innerDerived.disconnect()(engine)
  val innerDerived: rescala.reactives.Signals.SignalResource[T, S]
  override def interpretable: Interp[T, S] = innerDerived


  /** Returns the current value of the signal
    * However, using now is in most cases not what you want.
    * It does not build dependencies, does not integrate into transactions.
    * Use only for examples and debug output.
    * @group accessor */
  final def now: T = readValueOnce
  /** Returns the current value of the signal
    * @group accessor */
  final def readValueOnce: T = {
    RExceptions.toExternalReadException(this, scheduler.singleReadValueOnce(this))
  }


  /** add an observer
    *
    * @group accessor */
  final def observe(onValue: T => Unit,
                    onError: Throwable => Unit = null,
                    fireImmediately: Boolean = true)
                   (implicit ticket: CreationTicket)
  : Observe = Observe.strong(this.innerDerived, fireImmediately) { reevalVal =>
    new ObserveInteract {
      override def checkExceptionAndRemoval(): Boolean = {
        reevalVal match {
          case Pulse.empty                             => ()
          case Pulse.Exceptional(f) if onError == null =>
            throw ObservedException(Signal.this.innerDerived, "observed", f)
          case _                                       => ()
        }
        false
      }

      override def execute(): Unit = reevalVal match {
        case Pulse.empty => ()
        case Pulse.Value(v) => onValue(v)
        case Pulse.Exceptional(f) => onError(f)
      }
    }
  }

  /** Uses a partial function `onFailure` to recover an error carried by the event into a value. */
  @cutOutOfUserComputation
  final def recover[R >: T](onFailure: PartialFunction[Throwable, R])
                           (implicit ticket: CreationTicket)
  : Sig[R] = rescalaAPI.Signals.static(this.innerDerived) { st =>
    try st.dependStatic(this.innerDerived) catch {
      case NonFatal(e) => onFailure.applyOrElse[Throwable, R](e, throw _)
    }
  }

  // ================== Derivations ==================

  //final def recover[R >: A](onFailure: Throwable => R)(implicit ticket: TurnSource[S]): Sig[R] = recover(PartialFunction(onFailure))

  @cutOutOfUserComputation
  final def abortOnError(message: String)(implicit ticket: CreationTicket): Sig[T]
  = recover{case t => throw ObservedException(this.innerDerived, s"forced abort ($message)", t)}

  @cutOutOfUserComputation
  final def withDefault[R >: T](value: R)(implicit ticket: CreationTicket)
  : Sig[R] = rescalaAPI.Signals.static(this.innerDerived) { st =>
    try st.dependStatic(this.innerDerived) catch {
      case EmptySignalControlThrowable => value
    }
  }

  /** Return a Signal with f applied to the value
    * @group operator */
  @cutOutOfUserComputation
  final def map[B](expression: T => B)(implicit ticket: CreationTicket): Sig[B]
  = macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[T, B, S, rescala.reactives.Signals.MapFuncImpl.type, Signals.type]


  /** Flattens the inner value.
    * @group operator */
  @cutOutOfUserComputation
  final def flatten[R](implicit flatten: Flatten[Signal[T, S], R]): R = flatten.apply(this)

//  /** Delays this signal by n occurrences */
//  final def delay[A1 >: A](n: Int)(implicit ticket: CreationTicket], ev: ReSerializable[Queue[A1]]): Sig[A1] =
//    ticket { implicit ict => changed.delay[A1](ict.turn.staticBefore(this).get, n) }

  /** Create an event that fires every time the signal changes. It fires the tuple (oldVal, newVal) for the signal.
    * Be aware that no change will be triggered when the signal changes to or from empty
    * @group conversion */
  @cutOutOfUserComputation
  final def change(implicit ticket: CreationTicket): Event[Diff[T]] = rescalaAPI.Events.change(this)(ticket)

  /** Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    *
    * @group conversion */
  @cutOutOfUserComputation
  final def changed(implicit ticket: CreationTicket): Event[T]
  = rescalaAPI.Events.staticNamed(s"(changed $this)", this.innerDerived) { st =>
    st.collectStatic(this.innerDerived) match {
      case Pulse.empty => Pulse.NoChange
      case other => other
    }
  }

  /** Convenience function filtering to events which change this reactive to value
    * @group conversion */
  @cutOutOfUserComputation
  final def changedTo[V >: T](value: V)(implicit ticket: CreationTicket): Event[Unit]
  = rescalaAPI.Events.staticNamed(s"(filter $this)", this.innerDerived) { st =>
    st.collectStatic(this.innerDerived).filter(_ == value) }
    .dropParam
}

