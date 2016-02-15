package rescala.reactives

import rescala.engines.Ticket
import rescala.graph._
import rescala.propagation.Turn

object Signals extends GeneratedSignalLift {

  object Impl {
    private class StaticSignal[T, S <: Spores](_bud: S#StructP[T, Reactive[S]], expr: (Turn[S], T) => T)
      extends Base[T, S](_bud) with Signal[T, S] with StaticReevaluation[T, S] {

      override def calculatePulse()(implicit turn: Turn[S]): Pulse[T] = {
        val currentValue = pulses.base.current.get
        Pulse.diff(expr(turn, currentValue), currentValue)
      }
    }

    private class DynamicSignal[T, S <: Spores](bufferFactory: S, expr: Turn[S] => T) extends Base[T, S](bufferFactory.bud(transient = false)) with Signal[T, S] with DynamicReevaluation[T, S] {
      def calculatePulseDependencies(implicit turn: Turn[S]): (Pulse[T], Set[Reactive[S]]) = {
        val (newValue, dependencies) = turn.collectDependencies(expr(turn))
        (Pulse.diffPulse(newValue, pulses.base), dependencies)
      }
    }

    /** creates a signal that statically depends on the dependencies with a given initial value */
    def makeStatic[T, S <: Spores](dependencies: Set[Reactive[S]], init: => T)(expr: (Turn[S], T) => T)(initialTurn: Turn[S]): Signal[T, S] = initialTurn.create(dependencies) {
      val bud: S#StructP[T, Reactive[S]] = initialTurn.bufferFactory.bud(Pulse.unchanged(init), transient = false, initialIncoming = dependencies)
      new StaticSignal(bud, expr)
    }

    /** creates a dynamic signal */
    def makeDynamic[T, S <: Spores](dependencies: Set[Reactive[S]])(expr: Turn[S] => T)(initialTurn: Turn[S]): Signal[T, S] = initialTurn.create(dependencies, dynamic = true) {
      new DynamicSignal[T, S](initialTurn.bufferFactory, expr)
    }
  }


  /** creates a new static signal depending on the dependencies, reevaluating the function */
  def static[T, S <: Spores](dependencies: Reactive[S]*)(fun: Turn[S] => T)(implicit ticket: Ticket[S]): Signal[T, S] = ticket { initialTurn =>
    // using an anonymous function instead of ignore2 causes dependencies to be captured, which we want to avoid
    def ignore2[I, C, R](f: I => R): (I, C) => R = (t, _) => f(t)
    Impl.makeStatic(dependencies.toSet[Reactive[S]], fun(initialTurn))(ignore2(fun))(initialTurn)
  }

  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  def dynamic[T, S <: Spores](dependencies: Reactive[S]*)(expr: Turn[S] => T)(implicit ticket: Ticket[S]): Signal[T, S] =
    ticket(Impl.makeDynamic(dependencies.toSet[Reactive[S]])(expr)(_))

  /** creates a signal that folds the events in e */
  def fold[E, T, S <: Spores](e: Event[E, S], init: T)(f: (T, E) => T)(implicit ticket: Ticket[S]): Signal[T, S] = ticket { initialTurn =>
    Impl.makeStatic(Set[Reactive[S]](e), init) { (turn, currentValue) =>
      e.pulse(turn).fold(currentValue, f(currentValue, _))
    }(initialTurn)
  }

}
