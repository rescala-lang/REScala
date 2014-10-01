package rescala.signals

import rescala._
import rescala.propagation._

abstract class StaticDependentSignal[+T](creationTurn: Turn) extends Signal[T] {

  def initialValue()(implicit turn: Turn): T
  def calculateValue()(implicit turn: Turn): T

  currentValue = initialValue()(creationTurn)

  override def reevaluate()(implicit turn: Turn): EvaluationResult = {
    val newValue = calculateValue()
    val p = Pulse.diff(newValue, currentValue)
    pulse(p)
    EvaluationResult.Done(p.isChange, dependants)
  }
}
