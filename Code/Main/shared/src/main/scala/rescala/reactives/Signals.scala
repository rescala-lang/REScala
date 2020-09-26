package rescala.reactives

import rescala.core._
import rescala.interface.RescalaInterface
import rescala.macros.cutOutOfUserComputation

import scala.concurrent.{ExecutionContext, Future}

object Signals {
  type Sstate[T, S <: Struct] = S#State[Pulse[T], S]

  trait SignalResource[+T, S <: Struct] extends ReSource[S] with InterpMacro[T, S] with Disconnectable[S] {
    override type Value <: Pulse[T]
    override def interpret(v: Value): T                        = v.get
    override def resource: Interp[T, S]                        = this
    override protected[rescala] def commit(base: Value): Value = base
  }

  object MapFuncImpl { def apply[T1, A](value: T1, mapper: T1 => A): A = mapper(value) }

}

case class UserDefinedFunction[+T, Dep, Cap](
    staticDependencies: Set[Dep],
    expression: Cap => T,
    isStatic: Boolean = true
)
object UserDefinedFunction {
  implicit def fromExpression[T, Dep, Cap](expression: => T): UserDefinedFunction[T, Dep, Cap] =
    macro rescala.macros.ReactiveMacros.UDFExpressionWithAPI[T, Dep, Cap]
}

/** Functions to construct signals, you probably want to use signal expressions in [[rescala.interface.RescalaInterface.Signal]] for a nicer API. */
trait Signals[S <: Struct] {

  val rescalaAPI: RescalaInterface[S]
  private def ignore2[Tick, Current, Res](f: Tick => Res): (Tick, Current) => Res = (ticket, _) => f(ticket)

  import rescalaAPI.Impls.SignalImpl

  def wrapWithSignalAPI[T](derived: SignalImpl[T]): Signal[T, S] = {
    new Signal[T, S] {
      override val rescalaAPI: RescalaInterface[S]        = Signals.this.rescalaAPI
      override val resource: Signals.SignalResource[T, S] = derived
    }
  }

  @cutOutOfUserComputation
  def ofUDF[T](udf: UserDefinedFunction[T, ReSource[S], DynamicTicket[S]])(implicit
      ct: CreationTicket[S]
  ): Signal[T, S] = {
    val derived = ct.create[Pulse[T], SignalImpl[T]](udf.staticDependencies, Pulse.empty, inite = true) {
      state =>
        new SignalImpl[T](
          state,
          ignore2(udf.expression),
          ct.rename,
          if (udf.isStatic) None else Some(udf.staticDependencies)
        )
    }
    wrapWithSignalAPI(derived)
  }

  /** creates a new static signal depending on the dependencies, reevaluating the function */
  @cutOutOfUserComputation
  def static[T](dependencies: ReSource[S]*)(expr: StaticTicket[S] => T)(implicit
      ct: CreationTicket[S]
  ): Signal[T, S] = {
    val derived = ct.create[Pulse[T], SignalImpl[T]](dependencies.toSet, Pulse.empty, inite = true) {
      state => new SignalImpl[T](state, ignore2(expr), ct.rename, None)
    }
    wrapWithSignalAPI(derived)
  }

  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  @cutOutOfUserComputation
  def dynamic[T](dependencies: ReSource[S]*)(expr: DynamicTicket[S] => T)(implicit
      ct: CreationTicket[S]
  ): Signal[T, S] = {
    val staticDeps = dependencies.toSet
    val derived = ct.create[Pulse[T], SignalImpl[T]](staticDeps, Pulse.empty, inite = true) {
      state => new SignalImpl[T](state, ignore2(expr), ct.rename, Some(staticDeps))
    }
    wrapWithSignalAPI(derived)
  }

  /** converts a future to a signal */
  @cutOutOfUserComputation
  def fromFuture[A](fut: Future[A])(implicit fac: Scheduler[S], ec: ExecutionContext): Signal[A, S] = {
    val v: Var[A, S] = rescalaAPI.Var.empty[A]
    fut.onComplete { res => fac.forceNewTransaction(v)(t => v.admitPulse(Pulse.tryCatch(Pulse.Value(res.get)))(t)) }
    v
  }

  @cutOutOfUserComputation
  def lift[A, R](los: Seq[Signal[A, S]])(fun: Seq[A] => R)(implicit maybe: CreationTicket[S]): Signal[R, S] = {
    static(los.map(_.resource): _*) { t => fun(los.map(s => t.dependStatic(s.resource))) }
  }

  @cutOutOfUserComputation
  def lift[A1, B](n1: Signal[A1, S])(fun: A1 => B)(implicit maybe: CreationTicket[S]): Signal[B, S] = {
    static(n1.resource)(t => fun(t.dependStatic(n1.resource)))
  }

  @cutOutOfUserComputation
  def lift[A1, A2, B](n1: Signal[A1, S], n2: Signal[A2, S])(fun: (A1, A2) => B)(implicit
      maybe: CreationTicket[S]
  ): Signal[B, S] = {
    static(n1.resource, n2.resource)(t => fun(t.dependStatic(n1.resource), t.dependStatic(n2.resource)))
  }
}
