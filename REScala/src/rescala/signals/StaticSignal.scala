package rescala.signals

import rescala._
import rescala.propagation.{MaybeTurn, Turn}

/** A dependent reactive value which has static dependencies */
class StaticSignal[+T](dependencies: Set[Dependency[Any]])(expr: Turn => T)(creationTurn: Turn)
  extends StaticDependentSignal[T](creationTurn) {

  //if (dependencies.nonEmpty) staticDependencies(dependencies)(creationTurn)

  override def initialValue()(implicit turn: Turn): T = expr(creationTurn)
  override def calculateValue()(implicit turn: Turn): T = expr(turn)
}

/**
 * Create a StaticSignal
 */
object StaticSignal {

  def turn[T](dependencies: Set[Dependency[Any]])(expr: Turn => T)(implicit maybe: MaybeTurn): StaticSignal[T] = Turn.maybeTurn { turn =>
    val signal = new StaticSignal(dependencies)(expr)(turn)
    turn.register(signal, dependencies)
    signal
  }

  def turn[T](dependencies: Dependency[Any]*)(expr: Turn => T)(implicit maybe: MaybeTurn): StaticSignal[T] = turn(dependencies.toSet)(expr)

  def apply[T](dependencies: Set[Dependency[Any]])(expr: => T): StaticSignal[T] = turn(dependencies)(_ => expr)

  def apply[T](dependencies: Dependency[Any]*)(expr: => T): StaticSignal[T] = apply(dependencies.toSet)(expr)
}
