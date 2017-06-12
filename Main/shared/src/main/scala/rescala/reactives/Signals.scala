package rescala.reactives

import rescala.engine._
import rescala.graph._
import rescala.reactives.RExceptions.EmptySignalControlThrowable
import rescala.reactives.Signals.Impl.{DynamicSignal, StaticSignal, restored, states}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.DynamicVariable

object Signals extends GeneratedSignalLift {

  object Impl {

    /* TODO: this is currently used for meta research, but should be replaced with a sane implementation */

    val restored = new DynamicVariable[List[_]](null)
    val states = new DynamicVariable[List[Signal[_, _]]](null)

    def getStates[R](f: => R): (R, List[Signal[_, _]]) = states.withValue(Nil) {
      val res = f
      (res, states.value)
    }

    def restoreFrom[R, S <: Struct](states: List[Signal[_, _]])(f: => R)(implicit turnSource: CreationTicket[S]): R = turnSource { ctc =>
      restored.withValue(states.map(s => ctc.turn.dynamicBefore(s.asInstanceOf[Signal[_, S]]).get).reverse) {
        f
      }
    }

    /* TODO: end of meta hacks*/


    private[Signals] abstract class StaticSignal[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: (StaticTicket[S], => T) => T)
      extends Base[T, S](_bud) with Signal[T, S] {

      override protected[rescala] def reevaluate(turn: Turn[S]): ReevaluationResult[Value, S] = {
        val currentPulse: Pulse[T] = turn.selfBefore(this)
        def newValue = expr(turn.makeStaticReevaluationTicket(), currentPulse.get)
        val newPulse = Pulse.tryCatch(Pulse.diffPulse(newValue, currentPulse))
        ReevaluationResult.Static(newPulse)
      }
    }

    private[Signals] abstract class DynamicSignal[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: DynamicTicket[S] => T) extends Base[T, S](_bud) with Signal[T, S] {
      override protected[rescala] def reevaluate(turn: Turn[S]): ReevaluationResult[Value, S] = {
        val dt = turn.makeDynamicReevaluationTicket()
        val newPulse = Pulse.tryCatch { Pulse.diffPulse(expr(dt), turn.selfBefore(this)) }
        ReevaluationResult.Dynamic(newPulse, dt.collectedDependencies)
      }
    }


  }

  /** creates a signal that statically depends on the dependencies with a given initial value */
  private[rescala] def staticFold[T, S <: Struct](dependencies: Set[Reactive[S]], init: StaticTicket[S] => T)(expr: (StaticTicket[S], => T) => T)(ict: InnerCreationTicket[S]): Signal[T, S] = {
    def initOrRestored = {
      if (restored.value eq null) init(ict.turn.makeStaticReevaluationTicket())
      else {
        restored.value = restored.value.drop(1)
        restored.value.headOption.fold(init(ict.turn.makeStaticReevaluationTicket()))(_.asInstanceOf[T])
      }
    }
    val res = ict.create[Pulse[T], Signal[T, S]](dependencies, ValuePersistency.InitializedSignal(Pulse.tryCatch(Pulse.Value(initOrRestored)))) {
      state => new StaticSignal[T, S](state, expr) with Disconnectable[S]
    }
    if (states.value ne null) states.value = res :: states.value
    res
  }

  /** creates a new static signal depending on the dependencies, reevaluating the function */
  def static[T, S <: Struct](dependencies: Reactive[S]*)(expr: StaticTicket[S] => T)(implicit maybe: CreationTicket[S]): Signal[T, S] = maybe { initialTurn =>
    def ignore2[I, C, R](f: I => R): (I, C) => R = (t, _) => f(t)
    initialTurn.create[Pulse[T], Signal[T, S]](dependencies.toSet[Reactive[S]], ValuePersistency.DerivedSignal) {
      state => new StaticSignal[T, S](state, ignore2(expr)) with Disconnectable[S]
    }
  }

  def lift[A, S <: Struct, R](los: Seq[Signal[A, S]])(fun: Seq[A] => R)(implicit maybe: CreationTicket[S]): Signal[R, S] = {
    static(los: _*){t => fun(los.map(s => t.staticDepend(s).get))}
  }

  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  def dynamic[T, S <: Struct](dependencies: Reactive[S]*)(expr: DynamicTicket[S] => T)(implicit maybe: CreationTicket[S]): Signal[T, S] = maybe { initialTurn =>
    initialTurn.create[Pulse[T], Signal[T, S]](dependencies.toSet[Reactive[S]], ValuePersistency.DerivedSignal) {
      state => new DynamicSignal[T, S](state, expr) with Disconnectable[S]
    }
  }

  /** converts a future to a signal */
  def fromFuture[A, S <: Struct](fut: Future[A])(implicit fac: Engine[S], ec: ExecutionContext): Signal[A, S] = {
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
