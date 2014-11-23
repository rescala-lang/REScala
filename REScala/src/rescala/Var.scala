package rescala

import rescala.propagation._
import rescala.propagation.turns.Turn
import rescala.propagation.turns.creation.TurnFactory
import rescala.signals.Signal

/** A root Reactive value without dependencies which can be set */
final case class Var[T](initval: T) extends Signal[T] {
  pulses.default = Pulse.unchanged(initval)

  def update(newValue: T)(implicit fac: TurnFactory): Unit = set(newValue)
  def set(newValue: T)(implicit fac: TurnFactory): Unit = fac.newTurn { turn =>
    planUpdate(newValue)(turn)
  }

  def planUpdate(newValue: T)(implicit turn: Turn): Unit = {
    val p = Pulse.diff(newValue, get)
    if (p.isChange) {
      pulses.set(p)
      turn.enqueue(this)
    }
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult =
    EvaluationResult.Done(changed = true)
}
