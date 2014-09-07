package rescala.signals

import rescala._
import rescala.propagation.Turn

/**
 * Create a StaticVar
 */
object StaticVar {
  @deprecated("use VarSynt instead", since = "unknown")
  def apply[T](initialValue: T) = new VarSynt(initialValue)
}

/** A dependent reactive value which has static dependencies */
class StaticSignal[+T](dependencies: List[Dependency[Any]])(expr: => T)(creationTurn: Turn)
  extends DependentSignalImplementation[T](creationTurn) {

  setDependencies(dependencies)
  override def initialValue()(implicit turn: Turn): T = expr
  override def calculateNewValue()(implicit turn: Turn): T = expr
}

/**
 * Create a StaticSignal
 */
object StaticSignal {

  def apply[T](dependencies: List[Dependency[Any]])(expr: => T) = Turn.maybeTurn { turn =>
    new StaticSignal(dependencies)(expr)(turn)
  }

  def apply[T]()(expr: => T): DependentSignal[T] = apply(List())(expr)

  def apply[T](dependencies: Dependency[Any]*)(expr: => T): DependentSignal[T] = apply(dependencies.toList)(expr)
}
