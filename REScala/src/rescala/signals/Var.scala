package rescala.signals

import rescala.propagation._

/** A root Reactive value without dependencies which can be set */
object Var {
  def apply[T](initval: T): Var[T] = new Var(initval)
}

class Var[T](initval: T) extends Signal[T] {
  pulses.default = Pulse.unchanged(initval)

  final def update(newValue: T)(implicit fac: TurnFactory): Unit = set(newValue)
  def set(newValue: T)(implicit fac: TurnFactory): Unit = fac.newTurn { turn =>
    planUpdate(newValue)(turn)
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
