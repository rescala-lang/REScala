package rescala.operator

import rescala.interface.RescalaInterface
import rescala.operator.RExceptions.{EmptySignalControlThrowable, ObservedException}
import rescala.core.Core
import rescala.macros.MacroTags.{Static, Dynamic}
import rescala.macros.cutOutOfUserComputation

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success
import scala.util.control.NonFatal

object SignalMacroImpl {
  object MapFuncImpl { def apply[T1, A](value: T1, mapper: T1 => A): A = mapper(value) }
}

trait SignalApi {
  selfType: RescalaInterface with EventApi with SignalApi with Sources with DefaultImplementations with Observing
    with Core =>

  /** Time changing value derived from the dependencies.
    *
    * @tparam T Type stored by the signal
    * @tparam S Struct type used for the propagation of the signal
    * @groupname operator Signal operators
    * @groupprio operator 10
    * @groupname conversion Signal to Event conversions
    * @groupprio conversion 20
    * @groupname accessors Accessors and observers
    * @groupprio accessor 5
    */
  trait Signal[+T] extends Disconnectable with InterpMacro[T] {
    override type Value <: Pulse[T]
    override def interpret(v: Value): T                        = v.get
    override protected[rescala] def commit(base: Value): Value = base
    override def resource: Interp[T]                           = this

    /** Returns the current value of the signal
      * However, using now is in most cases not what you want.
      * It does not build dependencies, does not integrate into transactions.
      * Use only for examples and debug output.
      *
      * @group accessor
      */
    final def now: T = readValueOnce

    /** Returns the current value of the signal
      *
      * @group accessor
      */
    final def readValueOnce: T = {
      RExceptions.toExternalReadException(this, scheduler.singleReadValueOnce(this))
    }

    /** add an observer
      *
      * @group accessor
      */
    final def observe(onValue: T => Unit, onError: Throwable => Unit = null, fireImmediately: Boolean = true)(implicit
        ticket: CreationTicket
    ): Observe =
      Observe.strong(this, fireImmediately) { reevalVal =>
        new ObserveInteract {
          override def checkExceptionAndRemoval(): Boolean = {
            reevalVal match {
              case Pulse.empty => ()
              case Pulse.Exceptional(f) if onError == null =>
                throw ObservedException(Signal.this.resource, "observed", f)
              case _ => ()
            }
            false
          }

          override def execute(): Unit =
            (reevalVal: Pulse[T]) match {
              case Pulse.empty          => ()
              case Pulse.Value(v)       => onValue(v)
              case Pulse.Exceptional(f) => onError(f)
              case Pulse.NoChange       => ()
            }
        }
      }

    /** Uses a partial function `onFailure` to recover an error carried by the event into a value. */
    @cutOutOfUserComputation
    final def recover[R >: T](onFailure: PartialFunction[Throwable, R])(implicit ticket: CreationTicket): Signal[R] =
      Signals.static(this.resource) { st =>
        try st.dependStatic(this.resource)
        catch {
          case NonFatal(e) => onFailure.applyOrElse[Throwable, R](e, throw _)
        }
      }

    // ================== Derivations ==================

    //final def recover[R >: A](onFailure: Throwable => R)(implicit ticket: TurnSource): Signal[R, S = recover(PartialFunction(onFailure))

    @cutOutOfUserComputation
    final def abortOnError(message: String)(implicit ticket: CreationTicket): Signal[T] =
      recover { case t => throw ObservedException(this.resource, s"forced abort ($message)", t) }

    @cutOutOfUserComputation
    final def withDefault[R >: T](value: R)(implicit ticket: CreationTicket): Signal[R] =
      Signals.static(this.resource) { st =>
        try st.dependStatic(this.resource)
        catch {
          case EmptySignalControlThrowable => value
        }
      }

    /** Return a Signal with f applied to the value
      * @group operator
      */
    @cutOutOfUserComputation
    final def map[B](expression: T => B)(implicit ticket: CreationTicket): Signal[B] =
      macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[
        T,
        B,
        rescala.operator.SignalMacroImpl.MapFuncImpl.type,
        Signals.type,
        StaticTicket,
        DynamicTicket,
        CreationTicket,
        LowPriorityCreationImplicits
      ]

    /** Flattens the inner value.
      * @group operator
      */
    @cutOutOfUserComputation
    final def flatten[R](implicit flatten: Flatten[Signal[T], R]): R = flatten.apply(this)

//  /** Delays this signal by n occurrences */
//  final def delay[A1 >: A](n: Int)(implicit ticket: CreationTicket], ev: ReSerializable[Queue[A1]]): Signal[A1] =
//    ticket { implicit ict => changed.delay[A1](ict.turn.staticBefore(this).get, n) }

    /** Create an event that fires every time the signal changes. It fires the tuple (oldVal, newVal) for the signal.
      * Be aware that no change will be triggered when the signal changes to or from empty
      *
      * @group conversion
      */
    @cutOutOfUserComputation
    final def change(implicit ticket: CreationTicket): Event[Diff[T]] = Events.change(this)(ticket)

    /** Create an event that fires every time the signal changes. The value associated
      * to the event is the new value of the signal
      *
      * @group conversion
      */
    @cutOutOfUserComputation
    final def changed(implicit ticket: CreationTicket): Event[T] =
      Events.staticNamed(s"(changed $this)", this.resource) { st =>
        st.collectStatic(this) match {
          case Pulse.empty => Pulse.NoChange
          case other       => other
        }
      }

    /** Convenience function filtering to events which change this reactive to value
      *
      * @group conversion
      */
    @cutOutOfUserComputation
    final def changedTo[V >: T](value: V)(implicit ticket: CreationTicket): Event[Unit] =
      Events.staticNamed(s"(filter $this)", this) { st =>
        st.collectStatic(this).filter(_ == value)
      }.dropParam

  }

  /** A signal expression can be used to create signals accessing arbitrary other signals.
    * Use the apply method on a signal to access its value inside of a signal expression.
    * {{{
    * val a: Signal[Int]
    * val b: Signal[Int]
    * val result: Signal[String] = Signal { a().toString + b().toString}
    * }}}
    * @group create
    */
  object Signal {
    final def apply[A](expression: A)(implicit ticket: CreationTicket): Signal[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[
        A,
        Static,
        Signals.type,
        StaticTicket,
        DynamicTicket,
        CreationTicket,
        LowPriorityCreationImplicits
      ]
    final def static[A](expression: A)(implicit ticket: CreationTicket): Signal[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[
        A,
        Static,
        Signals.type,
        StaticTicket,
        DynamicTicket,
        CreationTicket,
        LowPriorityCreationImplicits
      ]
    final def dynamic[A](expression: A)(implicit ticket: CreationTicket): Signal[A] =
      macro rescala.macros.ReactiveMacros.ReactiveExpression[
        A,
        Dynamic,
        Signals.type,
        StaticTicket,
        DynamicTicket,
        CreationTicket,
        LowPriorityCreationImplicits
      ]
  }

  class UserDefinedFunction[+T, Dep, Cap](
      val staticDependencies: Set[Dep],
      val expression: Cap => T,
      val isStatic: Boolean = true
  )

  /** Functions to construct signals, you probably want to use signal expressions in [[rescala.interface.RescalaInterface.Signal]] for a nicer API. */
  object Signals {

    private def ignore2[Tick, Current, Res](f: Tick => Res): (Tick, Current) => Res = (ticket, _) => f(ticket)

    @cutOutOfUserComputation
    def ofUDF[T](udf: UserDefinedFunction[T, ReSource, DynamicTicket])(implicit
        ct: CreationTicket
    ): Signal[T] = {
      ct.create[Pulse[T], SignalImpl[T]](udf.staticDependencies, Pulse.empty, inite = true) {
        state =>
          new SignalImpl[T](
            state,
            ignore2(udf.expression),
            ct.rename,
            if (udf.isStatic) None else Some(udf.staticDependencies)
          )
      }
    }

    /** creates a new static signal depending on the dependencies, reevaluating the function */
    @cutOutOfUserComputation
    def static[T](dependencies: ReSource*)(expr: StaticTicket => T)(implicit
        ct: CreationTicket
    ): Signal[T] = {
      ct.create[Pulse[T], SignalImpl[T]](dependencies.toSet, Pulse.empty, inite = true) {
        state => new SignalImpl[T](state, ignore2(expr), ct.rename, None)
      }
    }

    /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
    @cutOutOfUserComputation
    def dynamic[T](dependencies: ReSource*)(expr: DynamicTicket => T)(implicit
        ct: CreationTicket
    ): Signal[T] = {
      val staticDeps = dependencies.toSet
      ct.create[Pulse[T], SignalImpl[T]](staticDeps, Pulse.empty, inite = true) {
        state => new SignalImpl[T](state, ignore2(expr), ct.rename, Some(staticDeps))
      }
    }

    /** converts a future to a signal */
    @cutOutOfUserComputation
    def fromFuture[A](fut: Future[A])(implicit fac: Scheduler, ec: ExecutionContext): Signal[A] = {
      fut.value match {
        case Some(Success(value)) => Var(value)(scheduler)
        case _ =>
          val v: Var[A] = Var.empty[A](scheduler)
          fut.onComplete { res =>
            fac.forceNewTransaction(v)(t => v.admitPulse(Pulse.tryCatch(Pulse.Value(res.get)))(t))
          }
          v
      }
    }

    @cutOutOfUserComputation
    def lift[A, R](los: Seq[Signal[A]])(fun: Seq[A] => R)(implicit maybe: CreationTicket): Signal[R] = {
      static(los.map(_.resource): _*) { t => fun(los.map(s => t.dependStatic(s.resource))) }
    }

    @cutOutOfUserComputation
    def lift[A1, B](n1: Signal[A1])(fun: A1 => B)(implicit maybe: CreationTicket): Signal[B] = {
      static(n1.resource)(t => fun(t.dependStatic(n1.resource)))
    }

    @cutOutOfUserComputation
    def lift[A1, A2, B](n1: Signal[A1], n2: Signal[A2])(fun: (A1, A2) => B)(implicit
        maybe: CreationTicket
    ): Signal[B] = {
      static(n1.resource, n2.resource)(t => fun(t.dependStatic(n1.resource), t.dependStatic(n2.resource)))
    }
  }

}
