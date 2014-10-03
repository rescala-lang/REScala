package rescala.signals

import rescala.propagation._

abstract class StaticDependentSignal[+T](creationTurn: Turn) extends Signal[T] with StaticReevaluation[T] {

  def initialValue()(implicit turn: Turn): T
  def calculateValue()(implicit turn: Turn): T

  final override protected[this] var currentValue = initialValue()(creationTurn)

  override def calculatePulse()(implicit turn: Turn): Pulse[T] = Pulse.diff(calculateValue(), currentValue)
}
