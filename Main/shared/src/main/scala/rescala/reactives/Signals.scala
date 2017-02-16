package rescala.reactives

import rescala.engine.{Engine, TurnSource}
import rescala.graph._
import rescala.propagation.Turn
import rescala.reactives.RExceptions.EmptySignalControlThrowable

import scala.collection.TraversableLike
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

object Signals extends GeneratedSignalLift {

  object Impl {
    private class StaticSignal[T, S <: Struct](_bud: S#StructType[T, Reactive[S]], expr: (Turn[S], => T) => T)
      extends Base[T, S](_bud) with Signal[T, S] with StaticReevaluation[T, S] {

      override def calculatePulse()(implicit turn: Turn[S]): Pulse[T] = {
        val currentPulse: Pulse[T] = stable
        def newValue = expr(turn, currentPulse.getS)
        Pulse.tryCatch(Pulse.diffPulse(newValue, currentPulse))
      }
    }

    private class DynamicSignal[T, S <: Struct](_bud: S#StructType[T, Reactive[S]], expr: Turn[S] => T) extends Base[T, S](_bud) with Signal[T, S] with DynamicReevaluation[T, S] {
      def calculatePulseDependencies(implicit turn: Turn[S]): (Pulse[T], Set[Reactive[S]]) = {
        turn.collectMarkedDependencies {
          Pulse.tryCatch { Pulse.diffPulse(expr(turn), stable) }
        }
      }
    }

    /** creates a signal that statically depends on the dependencies with a given initial value */
    def makeStatic[T, S <: Struct](dependencies: Set[Reactive[S]], init: => T)(expr: (Turn[S], => T) => T)(initialTurn: Turn[S]): Signal[T, S] = initialTurn.create(dependencies) {
      val bud: S#StructType[T, Reactive[S]] = initialTurn.makeStructState(Pulse.tryCatch(Pulse.Change(init)), transient = false, initialIncoming = dependencies)
      new StaticSignal(bud, expr)
    }

    /** creates a dynamic signal */
    def makeDynamic[T, S <: Struct](dependencies: Set[Reactive[S]])(expr: Turn[S] => T)(initialTurn: Turn[S]): Signal[T, S] = initialTurn.create(dependencies, dynamic = true) {
      val bud: S#StructType[T, Reactive[S]] = initialTurn.makeStructState(initialValue = Pulse.empty, transient = false)
      new DynamicSignal[T, S](bud, expr)
    }
  }


  /** creates a new static signal depending on the dependencies, reevaluating the function */
  def static[T, S <: Struct](dependencies: Reactive[S]*)(expr: Turn[S] => T)(implicit ticket: TurnSource[S]): Signal[T, S] = ticket { initialTurn =>
    // using an anonymous function instead of ignore2 causes dependencies to be captured, which we want to avoid
    def ignore2[I, C, R](f: I => R): (I, C) => R = (t, _) => f(t)
    Impl.makeStatic(dependencies.toSet[Reactive[S]], expr(initialTurn))(ignore2(expr))(initialTurn)
  }

  def lift[A, S <: Struct, R](los: Seq[Signal[A, S]])(fun: Seq[A] => R)(implicit ticket: TurnSource[S]): Signal[R, S] = {
    static(los: _*){t => fun(los.map(_.get(t)))}
  }

  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  def dynamic[T, S <: Struct](dependencies: Reactive[S]*)(expr: Turn[S] => T)(implicit ticket: TurnSource[S]): Signal[T, S] =
  ticket(Impl.makeDynamic(dependencies.toSet[Reactive[S]])(expr)(_))

  /** converts a future to a signal */
  def fromFuture[A, S <: Struct](fut: Future[A])(implicit fac: Engine[S, Turn[S]], ec: ExecutionContext): Signal[A, S] = {
    val v: Var[A, S] = rescala.reactives.Var.empty[A, S]
    fut.onComplete { res => fac.plan(v)(t => v.admitPulse(Pulse.tryCatch(Pulse.Change(res.get)))(t)) }
    v
  }

  case class Diff[+A](from: Pulse[A], to: Pulse[A]) {
    def _1: A = from.getS
    def _2: A = to.getS
    def pair: (A, A) = {
      try {
        val right = to.getS
        val left = from.getS
        left -> right
      } catch {
        case EmptySignalControlThrowable => throw new NoSuchElementException(s"Can not convert $this to pair")
      }
    }
  }

}
