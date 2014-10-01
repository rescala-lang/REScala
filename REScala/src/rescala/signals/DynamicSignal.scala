package rescala.signals

import rescala._
import rescala.propagation._


/** A dependant reactive value with dynamic dependencies (depending signals can change during evaluation) */
class DynamicSignal[+T]
    (expr: Turn => T)
  extends Signal[T] {

  def calculateValueDependencies(implicit turn: Turn): (T, Set[Dependency[_]]) =
    turn.dynamic.bag.withValue(Set()) { (expr(turn), turn.dynamic.bag.value) }

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult = {
    val (newValue, newDependencies) = calculateValueDependencies

    if (!turn.isReady(this, newDependencies)) {
      EvaluationResult.Retry(newDependencies)
    }
    else {
      val p = Pulse.diff(newValue, currentValue)
      pulse(p)
      EvaluationResult.Done(p.isChange, dependants, newDependencies)
    }
  }

}

/**
 * A syntactic signal
 */
object DynamicSignal {
  def apply[T](dependencies: Set[Dependency[Any]], expr: Turn => T): DynamicSignal[T] = Turn.maybeTurn { turn =>
    val signal = new DynamicSignal(expr)
    if (dependencies.nonEmpty) signal.ensureLevel(dependencies.map(_.level(turn)).max + 1)(turn)
    turn.evaluate(signal)
    signal
  }

  def apply[T](expr: Turn => T): DynamicSignal[T] = apply(Set(), expr)

  def apply[T](dependencies: List[Dependency[Any]])(expr: Turn => T): DynamicSignal[T] = apply(dependencies.toSet, expr)
  def apply[T](dependencies: Dependency[Any]*)(expr: Turn => T): DynamicSignal[T] = apply(dependencies.toSet, expr)

}
