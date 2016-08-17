package rescala.reactives

import rescala.engines.{Engine, Ticket}
import rescala.graph.Pulse.{Change, Exceptional, NoChange, Stable}
import rescala.graph._
import rescala.propagation.Turn
import rescala.reactives.RExceptions.EmptySignalControlThrowable

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import scala.util.{Failure, Success}

object Signals extends GeneratedSignalLift {

  object Impl {
    private class StaticSignal[T, S <: Struct](_bud: S#SporeP[T, Reactive[S]], expr: (Turn[S], => T) => T)
      extends Base[T, S](_bud) with Signal[T, S] with StaticReevaluation[T, S] {

      override def calculatePulse()(implicit turn: Turn[S]): Pulse[T] = {
        Pulse.tryCatch {
          val currentValue: Pulse[T] = pulses.base
          def theValue: T = currentValue match {
            case Stable(value) => value
            case Exceptional(t) => throw t
            case Change(value) => value
            case NoChange => throw new EmptySignalControlThrowable
          }
          Pulse.diffPulse(expr(turn, theValue), currentValue)
        }
      }
    }

    private class DynamicSignal[T, S <: Struct](_bud: S#SporeP[T, Reactive[S]], expr: Turn[S] => T) extends Base[T, S](_bud) with Signal[T, S] with DynamicReevaluation[T, S] {
      def calculatePulseDependencies(implicit turn: Turn[S]): (Pulse[T], Set[Reactive[S]]) = {
        val (newValueTry, dependencies) = turn.collectMarkedDependencies {RExceptions.reTry(expr(turn))}
        newValueTry match {
          case Success(p) => (Pulse.diffPulse(p, pulses.base), dependencies)
          case Failure(t) => (Pulse.Exceptional(t), dependencies)
        }
      }
    }

    /** creates a signal that statically depends on the dependencies with a given initial value */
    def makeStatic[T, S <: Struct](dependencies: Set[Reactive[S]], init: => T)(expr: (Turn[S], => T) => T)(initialTurn: Turn[S]): Signal[T, S] = initialTurn.create(dependencies) {
      val bud: S#SporeP[T, Reactive[S]] = initialTurn.bud(Pulse.tryCatch(Pulse.Stable(init)), transient = false, initialIncoming = dependencies)
      new StaticSignal(bud, expr)
    }

    /** creates a dynamic signal */
    def makeDynamic[T, S <: Struct](dependencies: Set[Reactive[S]])(expr: Turn[S] => T)(initialTurn: Turn[S]): Signal[T, S] = initialTurn.create(dependencies, dynamic = true) {
      val bud: S#SporeP[T, Reactive[S]] = initialTurn.bud(initialValue = Pulse.Exceptional(new EmptySignalControlThrowable), transient = false)
      new DynamicSignal[T, S](bud, expr)
    }
  }


  /** creates a new static signal depending on the dependencies, reevaluating the function */
  def static[T, S <: Struct](dependencies: Reactive[S]*)(expr: Turn[S] => T)(implicit ticket: Ticket[S]): Signal[T, S] = ticket { initialTurn =>
    // using an anonymous function instead of ignore2 causes dependencies to be captured, which we want to avoid
    def ignore2[I, C, R](f: I => R): (I, C) => R = (t, _) => f(t)
    Impl.makeStatic(dependencies.toSet[Reactive[S]], expr(initialTurn))(ignore2(expr))(initialTurn)
  }

  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  def dynamic[T, S <: Struct](dependencies: Reactive[S]*)(expr: Turn[S] => T)(implicit ticket: Ticket[S]): Signal[T, S] =
  ticket(Impl.makeDynamic(dependencies.toSet[Reactive[S]])(expr)(_))

  /** converts a future to a signal */
  def fromFuture[A, S <: Struct](fut: Future[A])(implicit fac: Engine[S, Turn[S]], ec: ExecutionContext): Signal[A, S] = {
    val v: Var[A, S] = rescala.reactives.Var.empty[A, S]
    fut.onComplete { res => fac.plan(v)(t => v.admitPulse(Pulse.tryCatch(Pulse.Change(res.get)))(t)) }
    v
  }

}
