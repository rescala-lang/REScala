package rescala.reactives

import rescala.core._
import rescala.interface.RescalaInterface
import rescala.macros.cutOutOfUserComputation
import rescala.reactives.Observe.ObserveInteract
import rescala.reactives.RExceptions.{EmptySignalControlThrowable, ObservedException}
import rescala.reactives.Signals.SignalResource

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
  implicit def signalResource[T, S <: Struct](signal: Signal[T, S]): SignalResource[T, S] = signal.resource
}
trait Signal[+T, S <: Struct] extends MacroInterp[T, S] with Disconnectable[S] {


  val rescalaAPI: RescalaInterface[S]
  import rescalaAPI.{scheduler, Observe, CreationTicket, Signal => Sig}


  override def disconnect()(implicit engine: Scheduler[S]): Unit = resource.disconnect()(engine)
  val resource: SignalResource[T, S]
  override def interpretable: Interp[T, S] = resource


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
  : Observe = Observe.strong(resource, fireImmediately) { reevalVal =>
    new ObserveInteract {
      override def checkExceptionAndRemoval(): Boolean = {
        reevalVal match {
          case Pulse.empty                             => ()
          case Pulse.Exceptional(f) if onError == null =>
            throw ObservedException(Signal.this.resource, "observed", f)
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
  : Sig[R] = rescalaAPI.Signals.static(this.resource) { st =>
    try st.dependStatic(this.resource) catch {
      case NonFatal(e) => onFailure.applyOrElse[Throwable, R](e, throw _)
    }
  }

  // ================== Derivations ==================

  //final def recover[R >: A](onFailure: Throwable => R)(implicit ticket: TurnSource[S]): Signal[R, S = recover(PartialFunction(onFailure))

  @cutOutOfUserComputation
  final def abortOnError(message: String)(implicit ticket: CreationTicket): Sig[T]
  = recover{case t => throw ObservedException(this.resource, s"forced abort ($message)", t)}

  @cutOutOfUserComputation
  final def withDefault[R >: T](value: R)(implicit ticket: CreationTicket)
  : Sig[R] = rescalaAPI.Signals.static(this.resource) { st =>
    try st.dependStatic(this.resource) catch {
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
  final def flatten[R](implicit flatten: Flatten[Sig[T], R]): R = flatten.apply(this)

//  /** Delays this signal by n occurrences */
//  final def delay[A1 >: A](n: Int)(implicit ticket: CreationTicket], ev: ReSerializable[Queue[A1]]): Sig[A1] =
//    ticket { implicit ict => changed.delay[A1](ict.turn.staticBefore(this).get, n) }

  /** Create an event that fires every time the signal changes. It fires the tuple (oldVal, newVal) for the signal.
    * Be aware that no change will be triggered when the signal changes to or from empty
    * @group conversion */
  @cutOutOfUserComputation
  final def change(implicit ticket: CreationTicket): Event[Diff[T], S] = rescalaAPI.Events.change(this)(ticket)

  /** Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    *
    * @group conversion */
  @cutOutOfUserComputation
  final def changed(implicit ticket: CreationTicket): Event[T, S]
  = rescalaAPI.Events.staticNamed(s"(changed $this)", this.resource) { st =>
    st.collectStatic(this.resource) match {
      case Pulse.empty => Pulse.NoChange
      case other => other
    }
  }

  /** Convenience function filtering to events which change this reactive to value
    * @group conversion */
  @cutOutOfUserComputation
  final def changedTo[V >: T](value: V)(implicit ticket: CreationTicket): Event[Unit, S]
  = rescalaAPI.Events.staticNamed(s"(filter $this)", this.resource) { st =>
    st.collectStatic(this.resource).filter(_ == value) }
    .dropParam


}
