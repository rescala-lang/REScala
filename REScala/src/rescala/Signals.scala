package rescala

import rescala.Signals.Impl.{makeDynamic, makeStatic}
import rescala.graph._
import rescala.signals.GeneratedLift
import rescala.turns.{Ticket, Turn}

object Signals extends GeneratedLift {

  object Impl {
    private class StaticSignal[T](engine: BufferFactory, dependencies: Set[Reactive], init: => T, expr: (Turn, T) => T)
      extends Base(engine, dependencies) with Signal[T] with StaticReevaluation[T] {

      pulses.initCurrent(Pulse.unchanged(init))

      override def calculatePulse()(implicit turn: Turn): Pulse[T] = {
        val currentValue = pulses.base.current.get
        Pulse.diff(expr(turn, currentValue), currentValue)
      }
    }

    private class DynamicSignal[T](bufferFactory: BufferFactory, expr: Turn => T) extends Base(bufferFactory) with Signal[T] with DynamicReevaluation[T] {
      def calculatePulseDependencies(implicit turn: Turn): (Pulse[T], Set[Reactive]) = {
        val (newValue, dependencies) = Globals.collectDependencies(expr(turn))
        (Pulse.diffPulse(newValue, pulses.base), dependencies)
      }
    }

    /** creates a signal that statically depends on the dependencies with a given initial value */
    def makeStatic[T](dependencies: Set[Reactive], init: => T)(expr: (Turn, T) => T)(initialTurn: Turn): Signal[T] = initialTurn.create(dependencies) {
      new StaticSignal(initialTurn.bufferFactory, dependencies, init, expr)
    }

    /** creates a dynamic signal */
    def makeDynamic[T](dependencies: Set[Reactive])(expr: Turn => T)(initialTurn: Turn): Signal[T] = initialTurn.create(dependencies, dynamic = true) {
      new DynamicSignal[T](initialTurn.bufferFactory, expr)
    }
  }


  /** creates a new static signal depending on the dependencies, reevaluating the function */
  def static[T](dependencies: Reactive*)(fun: Turn => T)(implicit ticket: Ticket): Signal[T] = ticket { initialTurn =>
    // using an anonymous function instead of ignore2 causes dependencies to be captured, which we want to avoid
    def ignore2[I, C, R](f: I => R): (I, C) => R = (t, _) => f(t)
    makeStatic(dependencies.toSet, fun(initialTurn))(ignore2(fun))(initialTurn)
  }

  /** creates a signal that has dynamic dependencies (which are detected at runtime with Signal.apply(turn)) */
  def dynamic[T](dependencies: Reactive*)(expr: Turn => T)(implicit ticket: Ticket): Signal[T] = ticket(makeDynamic(dependencies.toSet)(expr)(_))

  /** creates a signal that folds the events in e */
  def fold[E, T](e: Event[E], init: T)(f: (T, E) => T)(implicit ticket: Ticket): Signal[T] = ticket {
    makeStatic(Set(e), init) { (turn, currentValue) =>
      e.pulse(turn).fold(currentValue, f(currentValue, _))
    }(_)
  }

}
