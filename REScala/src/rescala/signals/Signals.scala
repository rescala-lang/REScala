package rescala.signals

import rescala.events.Event
import rescala.propagation.Pulse.{Diff, NoChange}
import rescala.propagation._

object Signals {

  def dynamic[T](dependencies: Set[Reactive])(expr: Turn => T)(implicit maybe: MaybeTurn): Signal[T] = maybe {
    _.create {
      new Signal[T] with DynamicReevaluation[T] {
        override protected[this] var currentValue: T = _
        def calculatePulseDependencies(implicit turn: Turn): (Pulse[T], Set[Reactive]) =
          turn.dynamic.bag.withValue(Set()) {
            val newValue = expr(turn)
            val dependencies = turn.dynamic.bag.value
            (Pulse.diff(newValue, currentValue), dependencies)
          }
      }
    } { (initialTurn, signal) =>
      if (dependencies.nonEmpty) signal.ensureLevel(dependencies.map(_.level(initialTurn)).max + 1)(initialTurn)
      initialTurn.evaluate(signal)
    }
  }

  def static[T](dependencies: Set[Reactive], init: T)(expr: (Turn, T) => T)(implicit maybe: MaybeTurn) = maybe {
    _.create(dependencies.toSet) {
      new Signal[T] with StaticReevaluation[T] {
        final override protected[this] var currentValue = init
        override def calculatePulse()(implicit turn: Turn): Pulse[T] = Pulse.diff(expr(turn, currentValue), currentValue)
      }
    }
  }

  def fold[E, T](e: Event[E], init: T)(f: (T, E) => T)(implicit maybe: MaybeTurn): Signal[T] =
    static(Set(e), init) { (turn, currentValue) =>
      e.pulse(turn).fold(currentValue)(f(currentValue, _))
    }

  def mapping[T](dependencies: Set[Reactive])(expr: Turn => T)(implicit maybe: MaybeTurn): Signal[T] = maybe { initialTurn =>
    static(dependencies, expr(initialTurn))((turn, _) => expr(turn))
  }

}
