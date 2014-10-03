package rescala.signals

import rescala.propagation._

abstract class StaticDependentSignal[+T](creationTurn: Turn) extends Signal[T] {

  def initialValue()(implicit turn: Turn): T
  def calculateValue()(implicit turn: Turn): T

  currentValue = initialValue()(creationTurn)

  override def calculatePulse()(implicit turn: Turn): Pulse[T] = Pulse.diff(calculateValue(), currentValue)
}
