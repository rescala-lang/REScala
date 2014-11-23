package rescala

import rescala.events.Event
import rescala.propagation.turns.Turn
import rescala.propagation.turns.creation.TurnFactory
import rescala.propagation.{EvaluationResult, Pulse}

/**
 * An implementation of an imperative event
 */
final case class Evt[T]() extends Event[T] {

  /** Trigger the event */
  def apply(v: T)(implicit fac: TurnFactory): Unit = fac.newTurn { turn =>
    pulses.set(Pulse.change(v))(turn)
    turn.enqueue(this)
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult =
    EvaluationResult.Done(changed = true)

  override def toString = getClass.getName
}