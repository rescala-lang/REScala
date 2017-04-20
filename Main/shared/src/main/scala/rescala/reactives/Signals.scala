package rescala.reactives

import rescala.engine.{Engine, TurnSource}
import rescala.graph._
import rescala.propagation.{DynamicTicket, StaticTicket, Turn}
import rescala.reactives.RExceptions.EmptySignalControlThrowable

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

object Signals extends GeneratedSignalLift {

  object Impl {
    private abstract class StaticSignal[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: (StaticTicket[S], => T) => T)
      extends Base[T, S](_bud) with Signal[T, S] {

      override protected[rescala] def reevaluate(ticket: S#Ticket[S]): ReevaluationResult[Value, S] = {
        val currentPulse: Pulse[T] = stable(ticket)
        def newValue = expr(ticket.static, currentPulse.get)
        val newPulse = Pulse.tryCatch(Pulse.diffPulse(newValue, currentPulse))
        ReevaluationResult.Static(newPulse)
      }
    }

    private abstract class DynamicSignal[T, S <: Struct](_bud: S#State[Pulse[T], S], expr: DynamicTicket[S] => T) extends Base[T, S](_bud) with Signal[T, S] {
      override protected[rescala] def reevaluate(ticket: S#Ticket[S]): ReevaluationResult[Value, S] = {
        val dt = ticket.dynamic()
        val newPulse = Pulse.tryCatch { Pulse.diffPulse(expr(dt), stable(ticket)) }
        ReevaluationResult.Dynamic(newPulse, dt.collectedDependencies)
      }
    }

    /** creates a signal that statically depends on the dependencies with a given initial value */
    def makeFold[T, S <: Struct](dependencies: Set[Reactive[S]], init: StaticTicket[S] => T)(expr: (StaticTicket[S], => T) => T)(initialTurn: Turn[S]): Signal[T, S] = initialTurn.create(dependencies) {
      val bud: S#State[Pulse[T], S] = initialTurn.makeStructState(Pulse.tryCatch(Pulse.Change(init(initialTurn.makeTicket().static()))), transient = false, initialIncoming = dependencies, hasState = true)
      new StaticSignal[T, S](bud, expr) with Disconnectable[S]
    }

    def makeStatic[T, S <: Struct](dependencies: Set[Reactive[S]], init: StaticTicket[S] => T)(expr: (StaticTicket[S], => T) => T)(initialTurn: Turn[S]): Signal[T, S] = initialTurn.create(dependencies) {
      val bud: S#State[Pulse[T], S] = initialTurn.makeStructState(Pulse.tryCatch(Pulse.Change(init(initialTurn.makeTicket().static()))), transient = false, initialIncoming = dependencies)
      new StaticSignal[T, S](bud, expr) with Disconnectable[S]
    }

    /** creates a dynamic signal */
    def makeDynamic[T, S <: Struct](dependencies: Set[Reactive[S]])(expr: DynamicTicket[S] => T)(initialTurn: Turn[S]): Signal[T, S] = initialTurn.create(dependencies, dynamic = true) {
      val bud: S#State[Pulse[T], S] = initialTurn.makeStructState(initialValue = Pulse.empty, transient = false)
      new DynamicSignal[T, S](bud, expr) with Disconnectable[S]
    }
  }


  /** creates a new static signal depending on the dependencies, reevaluating the function */
  def static[T, S <: Struct](dependencies: Reactive[S]*)(expr: StaticTicket[S] => T)(implicit ticket: TurnSource[S]): Signal[T, S] = ticket { initialTurn =>
    // using an anonymous function instead of ignore2 causes dependencies to be captured, which we want to avoid
    def ignore2[I, C, R](f: I => R): (I, C) => R = (t, _) => f(t)
    Impl.makeStatic(dependencies.toSet[Reactive[S]], expr)(ignore2(expr))(initialTurn)
  }

  def lift[A, S <: Struct, R](los: Seq[Signal[A, S]])(fun: Seq[A] => R)(implicit ticket: TurnSource[S]): Signal[R, S] = {
    static(los: _*){t => fun(los.map(_.pulse(t).get))}
  }

  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  def dynamic[T, S <: Struct](dependencies: Reactive[S]*)(expr: DynamicTicket[S] => T)(implicit ticket: TurnSource[S]): Signal[T, S] =
  ticket(Impl.makeDynamic(dependencies.toSet[Reactive[S]])(expr)(_))

  /** converts a future to a signal */
  def fromFuture[A, S <: Struct](fut: Future[A])(implicit fac: Engine[S, Turn[S]], ec: ExecutionContext): Signal[A, S] = {
    val v: Var[A, S] = rescala.reactives.Var.empty[A, S]
    fut.onComplete { res => fac.plan(v)(t => v.admitPulse(Pulse.tryCatch(Pulse.Change(res.get)))(t)) }
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
  }

  object Diff {
    def apply[A](from: Pulse[A], to: Pulse[A]): Diff[A] = new Diff(from, to)
    def unapply[A](arg: Diff[A]): Option[(A, A)] = arg.from match {
      case Pulse.Change(v1) => arg.to match {
        case Pulse.Change(v2) => Some((v1, v2))
        case _ => None
      }
      case _ => None
    }
  }

}
