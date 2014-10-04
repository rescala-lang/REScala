package rescala.signals

import rescala.events.Event
import rescala.propagation._

object Signals {

  def makeDynamic[T](dependencies: Set[Reactive])(expr: Turn => T)(implicit currentTurn: Turn): Signal[T] = currentTurn.create {
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

  def dynamic[T](dependencies: Reactive*)(expr: Turn => T)(implicit maybe: MaybeTurn): Signal[T] = maybe(makeDynamic(dependencies.toSet)(expr)(_))

  def makeStatic[T](dependencies: Set[Reactive], init: T)(expr: (Turn, T) => T)(implicit initialTurn: Turn) = initialTurn.create(dependencies.toSet) {
      new Signal[T] with StaticReevaluation[T] {
        final override protected[this] var currentValue = init
        override def calculatePulse()(implicit turn: Turn): Pulse[T] = Pulse.diff(expr(turn, currentValue), currentValue)
      }
    }

  def fold[E, T](e: Event[E], init: T)(f: (T, E) => T)(implicit maybe: MaybeTurn): Signal[T] = maybe {
    makeStatic(Set(e), init) { (turn, currentValue) =>
      e.pulse(turn).fold(currentValue)(f(currentValue, _))
    }(_)
  }

  def mapping[T](dependencies: Reactive*)(fun: Turn => T)(implicit maybe: MaybeTurn): Signal[T] = maybe { initialTurn =>
    makeStatic(dependencies.toSet, fun(initialTurn))((turn, _) => fun(turn))(initialTurn)
  }

  def lift[A1, A2, B](s1: Stateful[A1], s2: Stateful[A2])(fun: (A1, A2) => B)(implicit maybe: MaybeTurn): Signal[B] =
    mapping(s1, s2)(t => fun(s1.getValue(t), s2.getValue(t)))
  def lift[A1, A2, A3, B](s1: Stateful[A1], s2: Stateful[A2], s3: Stateful[A3])(fun: (A1, A2, A3) => B)(implicit maybe: MaybeTurn): Signal[B] =
    mapping(s1, s2)(t => fun(s1.getValue(t), s2.getValue(t), s3.getValue(t)))

}
