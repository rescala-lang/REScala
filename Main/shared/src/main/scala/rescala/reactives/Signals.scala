package rescala.reactives

import rescala.core._
import rescala.reactives.RExceptions.EmptySignalControlThrowable

import scala.concurrent.{ExecutionContext, Future}

/** Functions to construct signals, you probably want to signal expressions in [[rescala.RescalaInterface.Signal]] for a nicer API. */
object Signals {

  /** creates a signal that statically depends on the dependencies with a given initial value */
  def staticFold[T: ReSerializable, S <: Struct](dependencies: Set[ReSource[S]], init: Pulse[T])(expr: (StaticTicket[S], () => T) => T)(ict: Creation[S])(name: REName): Signal[T, S] = {
    ict.create[Pulse[T], StaticSignal[T, S]](dependencies, ValuePersistency.InitializedSignal[T](init)) {
      state => new StaticSignal[T, S](state, expr, name) with DisconnectableImpl[S]
    }
  }

  /** creates a new static signal depending on the dependencies, reevaluating the function */
  def static[T, S <: Struct](dependencies: ReSource[S]*)(expr: StaticTicket[S] => T)(implicit ct: CreationTicket[S]): Signal[T, S] = ct { initialTurn =>
    def ignore2[Ticket, Current, Result](f: Ticket => Result): (Ticket, Current) => Result = (ticket, _) => f(ticket)

    initialTurn.create[Pulse[T], StaticSignal[T, S]](dependencies.toSet, ValuePersistency.DerivedSignal) {
      state => new StaticSignal[T, S](state, ignore2(expr), ct.rename) with DisconnectableImpl[S]
    }
  }

  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  def dynamic[T, S <: Struct](dependencies: ReSource[S]*)(expr: DynamicTicket[S] => T)(implicit ct: CreationTicket[S]): Signal[T, S] = ct { initialTurn =>
    initialTurn.create[Pulse[T], DynamicSignal[T, S]](dependencies.toSet, ValuePersistency.DerivedSignal) {
      state => new DynamicSignal[T, S](state, expr, ct.rename) with DisconnectableImpl[S]
    }
  }

  /** converts a future to a signal */
  def fromFuture[A: ReSerializable, S <: Struct](fut: Future[A])(implicit fac: Scheduler[S], ec: ExecutionContext): Signal[A, S] = {
    val v: Var[A, S] = rescala.reactives.Var.empty[A, S]
    fut.onComplete { res => fac.transaction(v)(t => v.admitPulse(Pulse.tryCatch(Pulse.Value(res.get)))(t)) }
    v
  }

  def lift[A, S <: Struct, R](los: Seq[Signal[A, S]])(fun: Seq[A] => R)(implicit maybe: CreationTicket[S]): Signal[R, S] = {
    static(los: _*) { t => fun(los.map(s => t.staticDepend(s))) }
  }

  def lift[A1, B, S <: Struct](n1: Signal[A1, S])(fun: (A1) => B)(implicit maybe: CreationTicket[S]): Signal[B, S] = {
    static(n1)(t => fun(t.staticDepend(n1)))
  }

  def lift[A1, A2, B, S <: Struct](n1: Signal[A1, S], n2: Signal[A2, S])(fun: (A1, A2) => B)(implicit maybe: CreationTicket[S]): Signal[B, S] = {
    static(n1, n2)(t => fun(t.staticDepend(n1), t.staticDepend(n2)))
  }

  class Diff[+A](val from: Pulse[A], val to: Pulse[A]) {

    def _1: A = from.get
    def _2: A = to.get
    def pair: (A, A) = {
      try {
        val right = to.get
        val left = from.get
        left -> right
      } catch {
        case EmptySignalControlThrowable => throw new NoSuchElementException(s"Can not convert $this to pair")
      }
    }

    override def toString: String = s"Diff($from, $to)"
  }

  object Diff {
    def apply[A](from: Pulse[A], to: Pulse[A]): Diff[A] = new Diff(from, to)
    def unapply[A](arg: Diff[A]): Option[(A, A)] = arg.from match {
      case Pulse.Value(v1) => arg.to match {
        case Pulse.Value(v2) => Some((v1, v2))
        case _ => None
      }
      case _ => None
    }
  }

}


private abstract class StaticSignal[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: (StaticTicket[S], () => T) => T, name: REName)
  extends Base[Pulse[T], S](_bud, name) with Signal[T, S] {

  override protected[rescala] def reevaluate(st: DynamicTicket[S], before: Pulse[T]): ReevaluationResult[Value, S] = {
    def newValue = expr(st, () => before.get)
    val newPulse = Pulse.tryCatch(Pulse.diffPulse(newValue, before))

    ReevaluationResultWithValue.StaticPulse(newPulse)
  }
}

private abstract class DynamicSignal[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: DynamicTicket[S] => T, name: REName)
  extends Base[Pulse[T], S](_bud, name) with Signal[T, S] {

  override protected[rescala] def reevaluate(dt: DynamicTicket[S], before: Pulse[T]): ReevaluationResult[Value, S] = {
    dt.enableDynamic = true
    val newPulse = Pulse.tryCatch {Pulse.diffPulse(expr(dt), before)}
    ReevaluationResultWithValue.DynamicPulse(newPulse)
  }
}

