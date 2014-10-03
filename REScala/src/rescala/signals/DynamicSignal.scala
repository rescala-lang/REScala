package rescala.signals

import rescala.propagation._


/** A dependant reactive value with dynamic dependencies (depending signals can change during evaluation) */
final class DynamicSignal[+T]
    (expr: Turn => T)
  extends Signal[T] with DynamicReevaluation[T] {

  override protected[this] var currentValue: T = _

  def calculatePulseDependencies(implicit turn: Turn): (Pulse[T], Set[Reactive]) =
    turn.dynamic.bag.withValue(Set()) {
      val newValue = expr(turn)
      val dependencies = turn.dynamic.bag.value
      (Pulse.diff(newValue, currentValue), dependencies)
    }

}

/**
 * A syntactic signal
 */
object DynamicSignal {
  def apply[T](dependencies: Set[Pulsing[Any]], expr: Turn => T): DynamicSignal[T] = Turn.maybeTurn { turn =>
    val signal = new DynamicSignal(expr)
    if (dependencies.nonEmpty) signal.ensureLevel(dependencies.map(_.level(turn)).max + 1)(turn)
    turn.evaluate(signal)
    signal
  }

  def apply[T](expr: Turn => T): DynamicSignal[T] = apply(Set(), expr)

  def apply[T](dependencies: List[Pulsing[Any]])(expr: Turn => T): DynamicSignal[T] = apply(dependencies.toSet, expr)
  def apply[T](dependencies: Pulsing[Any]*)(expr: Turn => T): DynamicSignal[T] = apply(dependencies.toSet, expr)

}
