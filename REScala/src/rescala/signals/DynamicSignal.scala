package rescala.signals

import rescala._
import rescala.propagation._


/** A dependant reactive value with dynamic dependencies (depending signals can change during evaluation) */
class DynamicSignal[+T]
    (dependenciesUpperBound: List[Dependency[Any]])
    (expr: Turn => T)
    (creationTurn: Turn)
  extends Signal[T] {

  {
    val (newValue, newDependencies) = calculateValueDependencies(creationTurn)
    //setDependencies(newDependencies)(creationTurn)
    pulse(Pulse.change(newValue))(creationTurn)
    creationTurn.changed(this)
  }

  def calculateValueDependencies(implicit turn: Turn): (T, Set[Dependency[_]]) =
    turn.dynamic.bag.withValue(Set()) { (expr(turn), turn.dynamic.bag.value) }

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult = {
    val (newValue, newDependencies) = calculateValueDependencies

    val oldLevel = level

    //setDependencies(newDependencies)

    if (level > oldLevel) {
      EvaluationResult.Retry(newDependencies)
    }
    else {
      pulse(Pulse.diff(newValue, currentValue))
      EvaluationResult.Done(dependants)
    }
  }

}

/**
 * A syntactic signal
 */
object DynamicSignal {
  def apply[T](dependencies: List[Dependency[Any]])(expr: Turn => T): DynamicSignal[T] = Turn.maybeTurn { turn =>
    new DynamicSignal(dependencies)(expr)(turn)
  }

  def apply[T](expr: Turn => T): DynamicSignal[T] = apply(List())(expr)
  def apply[T](dependencyHolders: Dependency[Any]*)(expr: Turn => T): DynamicSignal[T] = apply(dependencyHolders.toList)(expr)

}
