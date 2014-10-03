package rescala.signals

import rescala.propagation.{MaybeTurn, Reactive, Turn}

/** A dependent reactive value which has static dependencies */
final class StaticSignal[+T](expr: Turn => T)(creationTurn: Turn)
  extends StaticDependentSignal[T](creationTurn) {

  //if (dependencies.nonEmpty) staticDependencies(dependencies)(creationTurn)

  override def initialValue()(implicit turn: Turn): T = expr(creationTurn)
  override def calculateValue()(implicit turn: Turn): T = expr(turn)
}

/**
 * Create a StaticSignal
 */
object StaticSignal {

  def turn[T](dependencies: Set[Reactive])(expr: Turn => T)(implicit maybe: MaybeTurn): StaticSignal[T] = Turn.maybeTurn { turn =>
    val signal = new StaticSignal(expr)(turn)
    turn.register(signal, dependencies)
    signal
  }

  def turn[T](dependencies: Reactive*)(expr: Turn => T)(implicit maybe: MaybeTurn): StaticSignal[T] = turn(dependencies.toSet)(expr)

  def apply[T](dependencies: Set[Reactive])(expr: => T): StaticSignal[T] = turn(dependencies)(_ => expr)

  def apply[T](dependencies: Reactive*)(expr: => T): StaticSignal[T] = apply(dependencies.toSet)(expr)
}
