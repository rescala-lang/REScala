package rescala.signals

import rescala._
import rescala.propagation.{NoChangePulse, DiffPulse, Turn}

abstract class DependentSignalImplementation[+T](creationTurn: Turn) extends DependentSignal[T] {

  def initialValue()(implicit turn: Turn): T
  def calculateNewValue()(implicit turn: Turn): T

  {
    log.nodeEvaluationStarted(this)
    currentValue = initialValue()(creationTurn)
    log.nodeEvaluationEnded(this)
  }

  override def triggerReevaluation()(implicit turn: Turn): Unit = {
    log.nodeEvaluationStarted(this)

    val oldLevel = level

     // Evaluation
    val newValue = try {
      calculateNewValue()
    }
    catch {
      case e: Exception =>
        log.nodeEvaluationEndedWithException(this, e)
        throw e
    }

    /* if the level increases by one, the dependencies might or might not have been evaluated this turn.
     * if they have, we could just fire the observers, but if they have not we are not allowed to do so
     *
     * if the level increases by more than one, we depend on something that still has to be in the queue
     */
    if (level == oldLevel + 1) {
      turn.addToEvalQueue(this)
    }
    else {
      if (level <= oldLevel) {
        /* Notify dependents only of the value changed */
        if (currentValue != newValue) {
          pulse(DiffPulse(newValue, currentValue))
        }
        else {
          pulse(NoChangePulse)
          log.nodePropagationStopped(this)
        }
      } : Unit
    }
    log.nodeEvaluationEnded(this)
  }
}
