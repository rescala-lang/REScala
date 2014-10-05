package rescala.signals

import rescala.propagation.{TurnLocal, EvaluationResult, Pulse, Turn}

/** A root Reactive value without dependencies which can be set */
object Var {
  def apply[T](initval: T): Var[T] = new Var(initval)
}

class Var[T](initval: T) extends Signal[T] {
  pulses.default = Pulse.unchanged(initval)

  final def update(newValue: T): Unit = set(newValue)
  def set(newValue: T): Unit = Turn.newTurn { turn =>
    planUpdate(newValue)(turn)
    turn.evaluateQueue()
  }

  def planUpdate(newValue: T)(implicit turn: Turn): Unit = {
    val p = Pulse.diff(newValue, getValue)
    if (p.isChange) {
      pulses.set(p)
      turn.enqueue(this)
    }
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult =
    EvaluationResult.Done(changed = true)
}
