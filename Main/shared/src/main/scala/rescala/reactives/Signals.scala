package rescala.reactives

import rescala.core._
import rescala.reactives.RExceptions.EmptySignalControlThrowable
import rescala.reactives.Signals.Impl.{DynamicSignal, StaticSignal}

import scala.concurrent.{ExecutionContext, Future}

object Signals extends GeneratedSignalLift {

  object Impl {

    private[Signals] abstract class StaticSignal[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: (StaticTicket[S], => T) => T, name: REName)
      extends Base[T, S](_bud, name) with Signal[T, S] {

      override protected[rescala] def reevaluate(turn: Turn[S], before: Pulse[T], indeps: Set[Reactive[S]]): ReevaluationResult[Value, S] = {
        def newValue = expr(turn.makeStaticReevaluationTicket(), before.get)
        val newPulse = Pulse.tryCatch(Pulse.diffPulse(newValue, before))
        ReevaluationResult.Static(newPulse, indeps)
      }
    }

    private[Signals] abstract class DynamicSignal[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: DynamicTicket[S] => T, name: REName) extends Base[T, S](_bud, name) with Signal[T, S] {
      override protected[rescala] def reevaluate(turn: Turn[S], before: Pulse[T], indeps: Set[Reactive[S]]): ReevaluationResult[Value, S] = {
        val dt = turn.makeDynamicReevaluationTicket(indeps)
        val newPulse = Pulse.tryCatch {Pulse.diffPulse(expr(dt), before)}
        ReevaluationResult.Dynamic(newPulse, dt.indepsAfter, dt.indepsAdded, dt.indepsRemoved)
      }
    }


  }

  /** creates a signal that statically depends on the dependencies with a given initial value */
  private[rescala] def staticFold[T: ReSerializable, S <: Struct](dependencies: Set[Reactive[S]], init: StaticTicket[S] => T)(expr: (StaticTicket[S], => T) => T)(ict: Creation[S])(name: REName): Signal[T, S] = {
    //TODO: should really not cast here … seemingly needed for snapshot semantics
    def initOrRestored: T = init(ict.asInstanceOf[TurnImpl[S]].makeStaticReevaluationTicket())
    val iorPulse: Pulse.Change[T] = Pulse.tryCatch(Pulse.Value(initOrRestored))
    ict.create[Pulse[T], StaticSignal[T, S]](dependencies, ValuePersistency.InitializedSignal[T](iorPulse)) {
      state => new StaticSignal[T, S](state, expr, name) with Disconnectable[S]
    }
  }

  /** creates a new static signal depending on the dependencies, reevaluating the function */
  def static[T, S <: Struct](dependencies: Reactive[S]*)(expr: StaticTicket[S] => T)(implicit ct: CreationTicket[S]): Signal[T, S] = ct { initialTurn =>
    def ignore2[I, C, R](f: I => R): (I, C) => R = (t, _) => f(t)
    initialTurn.create[Pulse[T], StaticSignal[T, S]](dependencies.toSet, ValuePersistency.DerivedSignal) {
      state => new StaticSignal[T, S](state, ignore2(expr), ct.rename) with Disconnectable[S]
    }
  }

  def lift[A, S <: Struct, R](los: Seq[Signal[A, S]])(fun: Seq[A] => R)(implicit maybe: CreationTicket[S]): Signal[R, S] = {
    static(los: _*) { t => fun(los.map(s => t.staticDepend(s).get)) }
  }

  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  def dynamic[T, S <: Struct](dependencies: Reactive[S]*)(expr: DynamicTicket[S] => T)(implicit ct: CreationTicket[S]): Signal[T, S] = ct { initialTurn =>
    initialTurn.create[Pulse[T], DynamicSignal[T, S]](dependencies.toSet, ValuePersistency.DerivedSignal) {
      state => new DynamicSignal[T, S](state, expr, ct.rename) with Disconnectable[S]
    }
  }

  /** converts a future to a signal */
  def fromFuture[A: ReSerializable, S <: Struct](fut: Future[A])(implicit fac: Engine[S], ec: ExecutionContext): Signal[A, S] = {
    val v: Var[A, S] = rescala.reactives.Var.empty[A, S]
    fut.onComplete { res => fac.transaction(v)(t => v.admitPulse(Pulse.tryCatch(Pulse.Value(res.get)))(t)) }
    v
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

    override def toString: String = "Diff" + pair
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
