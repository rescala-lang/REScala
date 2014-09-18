package rescala.signals

import rescala._
import rescala.propagation._

abstract class DependentSignal[+T](creationTurn: Turn) extends Signal[T] {

  def initialValue()(implicit turn: Turn): T
  def calculateValue()(implicit turn: Turn): T

  currentValue = initialValue()(creationTurn)

  override def reevaluate()(implicit turn: Turn): EvaluationResult = {
    val newValue = calculateValue()

    if (currentValue != newValue) pulse(DiffPulse(newValue, currentValue))
    else pulse(NoChangePulse)

    EvaluationResult.Done(dependants)
  }
}
