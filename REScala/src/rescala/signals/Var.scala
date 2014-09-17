package rescala.signals

import rescala.propagation.{EvaluationResult, DiffPulse, Turn}

object Var {
  def apply[T](initval: T): Var[T] = new Var(initval)

}

/** A root Reactive value without dependencies which can be set */
class Var[T](initval: T) extends Signal[T] {
  currentValue = initval

  final def update(newValue: T): Unit = set(newValue)
  def set(newValue: T): Unit = Turn.newTurn { turn =>
    if (currentValue != newValue) {
      planUpdate(newValue)(turn)
      turn.startEvaluation()
    }
  }

  def planUpdate(newValue: T)(implicit turn: Turn): Unit = {
    pulse(DiffPulse(newValue, currentValue))(turn)
    turn.evaluate(this)
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult =
    EvaluationResult.Done(dependants)
}
