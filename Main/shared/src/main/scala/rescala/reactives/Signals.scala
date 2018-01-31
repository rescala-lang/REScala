package rescala.reactives

import rescala.core._
import rescala.reactives.RExceptions.EmptySignalControlThrowable
import rescala.reactives.Signals.Sstate

import scala.concurrent.{ExecutionContext, Future}

/** Functions to construct signals, you probably want to use signal expressions in [[rescala.RescalaInterface.Signal]] for a nicer API. */
object Signals {
  type Sstate[S <: Struct, T] = S#State[Pulse[T], S, Nothing]

  /** creates a signal that statically depends on the dependencies with a given initial value */
  def staticFold[T: ReSerializable, S <: Struct](dependencies: Set[ReSource[S]],
                                                 init: Pulse[T])
                                                (expr: (StaticTicket[S], () => T) => T)
                                                (ict: Initializer[S])
                                                (name: REName): Signal[T, S] = {
    ict.create[Pulse[T], StaticSignal[T, S], Nothing](dependencies, Initializer.InitializedSignal[T](init)) {
      state => new StaticSignal[T, S](state, expr, name) with DisconnectableImpl[S]
    }
  }

  /** creates a new static signal depending on the dependencies, reevaluating the function */
  def static[T, S <: Struct](dependencies: ReSource[S]*)
                            (expr: StaticTicket[S] => T)
                            (implicit ct: CreationTicket[S]): Signal[T, S] =
    ct { initialTurn =>
      def ignore2[Tick, Current, Res](f: Tick => Res): (Tick, Current) => Res = (ticket, _) => f(ticket)

      initialTurn.create[Pulse[T], StaticSignal[T, S], Nothing](dependencies.toSet, Initializer.DerivedSignal) {
        state => new StaticSignal[T, S](state, ignore2(expr), ct.rename) with DisconnectableImpl[S]
      }
    }

  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  def dynamic[T, S <: Struct](dependencies: ReSource[S]*)
                             (expr: DynamicTicket[S] => T)
                             (implicit ct: CreationTicket[S]): Signal[T, S] =
    ct { initialTurn =>
      initialTurn.create[Pulse[T], DynamicSignal[T, S], Nothing](dependencies.toSet, Initializer.DerivedSignal) {
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
    static(los: _*) { t => fun(los.map(s => t.dependStatic(s))) }
  }

  def lift[A1, B, S <: Struct](n1: Signal[A1, S])(fun: (A1) => B)(implicit maybe: CreationTicket[S]): Signal[B, S] = {
    static(n1)(t => fun(t.dependStatic(n1)))
  }

  def lift[A1, A2, B, S <: Struct](n1: Signal[A1, S], n2: Signal[A2, S])(fun: (A1, A2) => B)(implicit maybe: CreationTicket[S]): Signal[B, S] = {
    static(n1, n2)(t => fun(t.dependStatic(n1), t.dependStatic(n2)))
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

  private[rescala] def computeNewValue[T, N, S <: Struct](rein: ReevTicket[Pulse[T], N, S], newValue: () => T): ReevTicket[Pulse[T], N, S] = {
    val newPulse = Pulse.tryCatch(Pulse.diffPulse(newValue(), rein.before))
    if (newPulse.isChange) rein.withValue(newPulse) else rein
  }

}


private abstract class StaticSignal[T, S <: Struct](_bud: Sstate[S, T], expr: (StaticTicket[S], () => T) => T, name: REName)
  extends Base[Pulse[T], S, Nothing](_bud, name) with Signal[T, S] {

  override protected[rescala] def reevaluate(rein: ReIn): Rout = {
    Signals.computeNewValue[T, Nothing, S](rein, () => expr(rein, () => rein.before.get))
  }

}

private abstract class DynamicSignal[T, S <: Struct](_bud: Sstate[S, T], expr: DynamicTicket[S] => T, name: REName)
  extends Base[Pulse[T], S, Nothing](_bud, name) with Signal[T, S] {

  override protected[rescala] def reevaluate(rein: ReIn): Rout = {
    rein.trackDependencies()
    Signals.computeNewValue[T, Nothing, S](rein, () => expr(rein))
  }
}

