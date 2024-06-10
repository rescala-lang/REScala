package reactives.operator

import reactives.core.*
import reactives.macros.MacroAccess
import reactives.SelectedScheduler.State
import reactives.structure.RExceptions.{EmptySignalControlThrowable, ObservedException}
import reactives.structure.{Diff, Observe, Pulse, RExceptions, SignalImpl}

import scala.annotation.nowarn
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success
import scala.util.control.NonFatal

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
  override type State[V] = reactives.SelectedScheduler.State[V]
  override type Value <: Pulse[T]
  override def read(v: Value): T = v.get

  given Conversion[Value, T] = _.get

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
    RExceptions.toExternalReadException(this, reactives.SelectedScheduler.candidate.scheduler.singleReadValueOnce(this))
  }

  /** add an observer
    *
    * @group accessor
    */
  final infix def observe(onValue: T => Unit, onError: Throwable => Unit = null, fireImmediately: Boolean = true)(
      using ticket: CreationTicket[State]
  ): Disconnectable =
    Observe.strong(this, fireImmediately) { reevalVal => new Observe.ObservePulsing(reevalVal, this, onValue, onError) }

  /** Uses a partial function `onFailure` to recover an error carried by the event into a value. */
  final def recover[R >: T](onFailure: PartialFunction[Throwable, R])(using
      ticket: CreationTicket[State]
  ): Signal[R] =
    Signal {
      try this.value
      catch {
        case NonFatal(e) => onFailure.applyOrElse[Throwable, R](e, throw _)
      }
    }

  /** Adds another error message in case this signal is empty, also disallows handling exceptions in observers */
  final def abortOnError(message: String)(using ticket: CreationTicket[State]): Signal[T] =
    recover { case t => throw ObservedException(this, s"forced abort ($message)", t) }

  /** Sets a default value in case this signal is empty.
    * @group operator
    */
  final def withDefault[R >: T](value: R)(using ticket: CreationTicket[State]): Signal[R] =
    Signal {
      try this.value
      catch {
        case e: EmptySignalControlThrowable => value
      }
    }

  /** Flattens the inner value.
    * @group operator
    */
  final def flatten[R](using flatten: Flatten[Signal[T], R]): R = flatten.apply(this)

  /** Create an event that fires every time the signal changes. It fires the tuple (oldVal, newVal) for the signal.
    * Be aware that no change will be triggered when the signal changes to or from empty
    *
    * @group conversion
    */
  final def change(using ticket: CreationTicket[State]): Event[Diff[T]] = Event.Impl.change(this)(using ticket)

  /** Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    *
    * @group conversion
    */
  final def changed(using ticket: CreationTicket[State]): Event[T] =
    Event.Impl.staticNamed(s"(changed $this)", this) { st =>
      st.collectStatic(this) match {
        case Pulse.empty(info) => Pulse.NoChange
        case other             => other
      }
    }

  /** Return a Signal with f applied to the value
    *
    * @group operator
    */
  final inline infix def map[B](inline expression: T => B)(using CreationTicket[State]): Signal[B] =
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

  inline def apply[T](inline expr: T)(using CreationTicket[State]): Signal[T] = static(expr)

  inline def static[T](inline expr: T)(using CreationTicket[State]): Signal[T] = {
    val (inputs, fun, isStatic) =
      reactives.macros.MacroLegos.getDependencies[T, ReSource.of[State], reactives.core.StaticTicket[State], true](expr)
    Signal.static(inputs*)(fun)
  }

  inline def dynamic[T](inline expr: T)(using CreationTicket[State]): Signal[T] = {
    val (sources, fun, isStatic) =
      reactives.macros.MacroLegos.getDependencies[T, ReSource.of[State], reactives.core.DynamicTicket[State], false](
        expr
      )
    Signal.dynamic(sources*)(fun)
  }

  /** creates a new static signal depending on the dependencies, reevaluating the function */
  def static[T](dependencies: ReSource.of[State]*)(expr: StaticTicket[State] => T)(using
      ct: CreationTicket[State]
  ): Signal[T] = {
    ct.scope.create[Pulse[T], SignalImpl[T] & Signal[T]](
      dependencies.toSet,
      Pulse.empty(ct.info),
      needsReevaluation = true
    ) {
      state => new SignalImpl(state, (t, _) => expr(t), ct.info, None) with Signal[T]
    }
  }

  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  def dynamic[T](dependencies: ReSource.of[State]*)(expr: DynamicTicket[State] => T)(using
      ct: CreationTicket[State]
  ): Signal[T] = {
    val staticDeps = dependencies.toSet
    ct.scope.create[Pulse[T], SignalImpl[T] & Signal[T]](
      staticDeps,
      Pulse.empty(ct.info),
      needsReevaluation = true
    ) {
      state => new SignalImpl(state, (t, _) => expr(t), ct.info, Some(staticDeps)) with Signal[T]
    }
  }

  /** converts a future to a signal */
  def fromFuture[A](fut: Future[A])(using
      creationScope: CreationScope[State],
      planScope: PlanTransactionScope[State],
      ec: ExecutionContext,
      name: ReInfo
  ): Signal[A] = {
    val creationTicket =
      new CreationTicket[State](creationScope, name.derive("fromFuture"))
    fut.value match {
      case Some(Success(value)) =>
        Var(value)(using creationTicket)
      case _ =>
        val v: Var[A] = Var.empty[A](using creationTicket)
        fut.onComplete { res =>
          planScope.planTransaction(v)(t => v.admitPulse(Pulse.tryCatch(Pulse.Value(res.get)))(using t))
        }
        v
    }
  }

  def lift[A, R](los: Seq[Signal[A]])(fun: Seq[A] => R)(using maybe: CreationTicket[State]): Signal[R] = {
    Signal.static(los*) { t => fun(los.map(s => t.dependStatic(s))) }
  }

  def lift[A1, B](n1: Signal[A1])(fun: A1 => B)(using CreationTicket[State]): Signal[B] =
    Signal { fun(n1.value) }

  def lift[A1, A2, B](n1: Signal[A1], n2: Signal[A2])(fun: (A1, A2) => B)(using
      CreationTicket[State]
  ): Signal[B] = {
    Signal { fun(n1.value, n2.value) }
  }

}
