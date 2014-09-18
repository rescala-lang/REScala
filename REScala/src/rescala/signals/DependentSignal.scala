package rescala.signals

import rescala._
import rescala.propagation._

abstract class DependentSignal[+T](creationTurn: Turn) extends Signal[T] {

  def initialValue()(implicit turn: Turn): T
  def calculateValue()(implicit turn: Turn): T

  currentValue = initialValue()(creationTurn)

  override def reevaluate()(implicit turn: Turn): EvaluationResult = {
    val newValue = calculateValue()
    pulse(Pulse.diff(newValue, currentValue))
    EvaluationResult.Done(dependants)
  }
}
