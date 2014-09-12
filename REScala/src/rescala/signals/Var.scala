package rescala.signals

import rescala.propagation.{DiffPulse, Turn}

object Var {
  def apply[T](initval: T): Var[T] = new Var[T] {
    currentValue = initval

    def set(newValue: T): Unit = Turn.newTurn { turn =>
      log.nodeValueSet(this)
      if (currentValue != newValue) {
        pulse(DiffPulse(newValue, currentValue))(turn)
        currentValue = newValue
        turn.startEvaluation()
      } else {
        log.nodePropagationStopped(this)
      }
    }
  }

}

/** A root Reactive value without dependencies which can be set */
trait Var[T] extends Signal[T] {
  def set(newValue: T): Unit
  final def update(newValue: T): Unit = set(newValue)
}
