package rescala.signals

import rescala._
import rescala.propagation.Turn


/** A dependant reactive value with dynamic dependencies (depending signals can change during evaluation) */
class DynamicSignal[+T]
    (dependenciesUpperBound: List[Dependency[Any]])
    (expr: Turn => T)
    (creationTurn: Turn)
  extends DependentSignalImplementation[T](creationTurn) with Dependant {

  override def initialValue()(implicit turn: Turn): T = calculateValue()

  override def calculateValue()(implicit turn: Turn): T = {
    turn.dynamic.bag.withValue(Set()) {
      val newValue = expr(turn)
      setDependencies(turn.dynamic.bag.value)
      newValue
    }
  }

  if(dependenciesUpperBound.nonEmpty) ensureLevel(dependenciesUpperBound.map{_.level(creationTurn)}.max)(creationTurn)

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
