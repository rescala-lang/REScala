package rescala.signals

import rescala._
import rescala.propagation.{NoChangePulse, DiffPulse, Turn}




/** A dependant reactive value with dynamic dependencies (depending signals can change during evaluation) */
class DynamicSignal[+T](
    dependenciesUpperBound: List[Dependency[Any]],
    private var detectedDependencies: Set[Dependency[Any]] = Set())
    (expr: DynamicSignal[T] => T)
    (creationTurn: Turn)
  extends DependentSignalImplementation[T](creationTurn) {

  override def onDynamicDependencyUse[A](dependency: Signal[A]): Unit = {
    super.onDynamicDependencyUse(dependency)
    detectedDependencies += dependency
  }

  override def initialValue()(implicit turn: Turn): T = calculateNewValue()

  override def calculateNewValue()(implicit turn: Turn): T = {
    val newValue = expr(this)
    setDependencies(detectedDependencies)
    detectedDependencies = Set()
    newValue
  }

  if(dependenciesUpperBound.nonEmpty) ensureLevel(dependenciesUpperBound.map{_.level}.max)

}

/**
 * A syntactic signal
 */
object DynamicSignal {
  def apply[T](dependencies: List[Dependency[Any]])(expr: DynamicSignal[T] => T) = Turn.maybeTurn { turn =>
    new DynamicSignal(dependencies)(expr)(turn)
  }

  def apply[T](expr: DynamicSignal[T] => T): DynamicSignal[T] = apply(List())(expr)
  def apply[T](dependencyHolders: Dependency[Any]*)(expr: DynamicSignal[T] => T): DynamicSignal[T] = apply(dependencyHolders.toList)(expr)

}
