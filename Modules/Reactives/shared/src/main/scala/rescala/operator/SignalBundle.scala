package rescala.operator

import rescala.core.*
import rescala.macros.MacroAccess
import rescala.structure.RExceptions.{EmptySignalControlThrowable, ObservedException}
import rescala.structure.{Diff, Observe, Pulse, RExceptions, SignalImpl}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success
import scala.util.control.NonFatal

trait SignalBundle {
  selfType: Operators =>

  /** Time changing value derived from the dependencies.
    *
    * @tparam T Type stored by the signal
    * @groupname operator Signal operators
    * @groupprio operator 10
    * @groupname conversion Signal to Event conversions
    * @groupprio conversion 20
    * @groupname accessors Accessors and observers
    * @groupprio accessor 5
    */
  trait Signal[+T] extends Disconnectable with MacroAccess[T] with ReSource {
    override type State[V] = selfType.BundleState[V]
    override type Value <: Pulse[T]
    override def read(v: Value): T                             = v.get
    override protected[rescala] def commit(base: Value): Value = base

    given Conversion[Value, T] = _.get

    /** Returns the current value of the signal
      * However, using now is in most cases not what you want.
      * It does not build dependencies, does not integrate into transactions.
      * Use only for examples and debug output.
      *
      * @group accessor
      */
    final def now(implicit scheduler: Scheduler[State]): T = readValueOnce

    /** Returns the current value of the signal
      *
      * @group accessor
      */
    final def readValueOnce(implicit scheduler: Scheduler[State]): T = {
      RExceptions.toExternalReadException(this, scheduler.singleReadValueOnce(this))
    }

    /** add an observer
      *
      * @group accessor
      */
    final infix def observe(onValue: T => Unit, onError: Throwable => Unit = null, fireImmediately: Boolean = true)(implicit
        ticket: CreationTicket[State]
    ): Disconnectable =
      Observe.strong(this, fireImmediately) { reevalVal =>
        new Observe.ObserveInteract {
          override def checkExceptionAndRemoval(): Boolean = {
            reevalVal match {
              case Pulse.empty => ()
              case Pulse.Exceptional(f) if onError == null =>
                throw ObservedException(Signal.this, "observed", f)
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
    final def recover[R >: T](onFailure: PartialFunction[Throwable, R])(implicit
        ticket: CreationTicket[State]
    ): Signal[R] =
      Signal {
        try this.value
        catch {
          case NonFatal(e) => onFailure.applyOrElse[Throwable, R](e, throw _)
        }
      }

    /** Adds another error message in case this signal is empty, also disallows handling exceptions in observers */
    final def abortOnError(message: String)(implicit ticket: CreationTicket[State]): Signal[T] =
      recover { case t => throw ObservedException(this, s"forced abort ($message)", t) }

    /** Sets a default value in case this signal is empty.
      * @group operator
      */
    final def withDefault[R >: T](value: R)(implicit ticket: CreationTicket[State]): Signal[R] =
      Signal {
        try this.value
        catch {
          case EmptySignalControlThrowable => value
        }
      }

    /** Flattens the inner value.
      * @group operator
      */
    final def flatten[R](implicit flatten: Flatten[Signal[T], R]): R = flatten.apply(this)

    /** Create an event that fires every time the signal changes. It fires the tuple (oldVal, newVal) for the signal.
      * Be aware that no change will be triggered when the signal changes to or from empty
      *
      * @group conversion
      */
    final def change(implicit ticket: CreationTicket[State]): Event[Diff[T]] = Events.change(this)(ticket)

    /** Create an event that fires every time the signal changes. The value associated
      * to the event is the new value of the signal
      *
      * @group conversion
      */
    final def changed(implicit ticket: CreationTicket[State]): Event[T] =
      Events.staticNamed(s"(changed $this)", this) { st =>
        st.collectStatic(this) match {
          case Pulse.empty => Pulse.NoChange
          case other       => other
        }
      }

    /** Return a Signal with f applied to the value
      *
      * @group operator
      */
    final inline infix def map[B](using CreationTicket[State])(inline expression: T => B): Signal[B] =
      Signal.dynamic(expression(this.value))
  }

  /** A signal expression can be used to create signals accessing arbitrary other signals.
    * Use the apply method on a signal to access its value inside of a signal expression.
    * {{{
    * val a: Signal[Int]
    * val b: Signal[Int]
    * val result: Signal[String] = Signal { a().toString + b().toString}
    * }}}
    *
    * @group create
    */
  object Signal {

    inline def apply[T](using CreationTicket[BundleState])(inline expr: T): Signal[T] = static(expr)

    inline def static[T](using CreationTicket[BundleState])(inline expr: T): Signal[T] = {
      val (inputs, fun, isStatic) =
        rescala.macros.getDependencies[T, ReSource.of[BundleState], rescala.core.StaticTicket[BundleState], true](expr)
      Signal.static(inputs*)(fun)
    }

    inline def dynamic[T](using CreationTicket[BundleState])(inline expr: T): Signal[T] = {
      val (sources, fun, isStatic) =
        rescala.macros.getDependencies[T, ReSource.of[BundleState], rescala.core.DynamicTicket[BundleState], false](
          expr
        )
      Signal.dynamic(sources*)(fun)
    }

    /** creates a new static signal depending on the dependencies, reevaluating the function */
    def static[T](dependencies: ReSource.of[BundleState]*)(expr: StaticTicket[BundleState] => T)(implicit
        ct: CreationTicket[BundleState]
    ): Signal[T] = {
      ct.create[Pulse[T], SignalImpl[BundleState, T] & Signal[T]](
        dependencies.toSet,
        Pulse.empty,
        needsReevaluation = true
      ) {
        state => new SignalImpl(state, (t, _) => expr(t), ct.info, None) with Signal[T]
      }
    }

    /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
    def dynamic[T](dependencies: ReSource.of[BundleState]*)(expr: DynamicTicket[BundleState] => T)(implicit
        ct: CreationTicket[BundleState]
    ): Signal[T] = {
      val staticDeps = dependencies.toSet
      ct.create[Pulse[T], SignalImpl[BundleState, T] & Signal[T]](
        staticDeps,
        Pulse.empty,
        needsReevaluation = true
      ) {
        state => new SignalImpl(state, (t, _) => expr(t), ct.info, Some(staticDeps)) with Signal[T]
      }
    }

    /** converts a future to a signal */
    def fromFuture[A](fut: Future[A])(implicit
        scheduler: Scheduler[BundleState],
        ec: ExecutionContext,
        name: ReInfo
    ): Signal[A] = {
      val creationTicket =
        new CreationTicket[BundleState](ScopeSearch.fromSchedulerImplicit(scheduler), name.derive("fromFuture"))
      fut.value match {
        case Some(Success(value)) =>
          Var(value)(creationTicket)
        case _ =>
          val v: Var[A] = Var.empty[A](creationTicket)
          fut.onComplete { res =>
            def execTx() =
              scheduler.forceNewTransaction(v)(t => v.admitPulse(Pulse.tryCatch(Pulse.Value(res.get)))(t))
            // The code below handles the extremely rare case, that the future completes within the currently running transaction.
            // As far as I can tell, that can only happen if the passed execution context executes the onComplete handler immediately.
            // There are contexts that do so for efficiency reasons.
            scheduler.maybeTransaction match {
              case None => execTx()
              case Some(tx) =>
                tx.observe(new Observation {
                  override def execute(): Unit = execTx()
                })
            }
          }
          v
      }
    }

    def lift[A, R](los: Seq[Signal[A]])(fun: Seq[A] => R)(implicit maybe: CreationTicket[BundleState]): Signal[R] = {
      Signal.static(los*) { t => fun(los.map(s => t.dependStatic(s))) }
    }

    def lift[A1, B](n1: Signal[A1])(fun: A1 => B)(using CreationTicket[BundleState]): Signal[B] =
      Signal { fun(n1.value) }

    def lift[A1, A2, B](n1: Signal[A1], n2: Signal[A2])(fun: (A1, A2) => B)(using
        CreationTicket[BundleState]
    ): Signal[B] = {
      Signal { fun(n1.value, n2.value) }
    }

  }

}
