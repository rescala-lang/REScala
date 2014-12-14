package rescala

import rescala.turns.{Turn, Engine}
import rescala.graph.{EvaluationResult, Pulse}

sealed trait Source[T] {
  def admit(value: T)(implicit turn: Turn): Unit
}

/**
 * An implementation of an imperative event
 */
final class Evt[T]() extends Event[T] with Source[T] {

  /** Trigger the event */
  def apply(value: T)(implicit fac: Engine): Unit = fac.plan { admit(value)(_) }

  def admit(value: T)(implicit turn: Turn): Unit =
    turn.admit(this) {
      pulses.set(Pulse.change(value))(turn)
    }

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult =
    EvaluationResult.Static(changed = pulse.isChange)
}

object Evt {
  def apply[T]() = new Evt[T]()
}



/** A root Reactive value without dependencies which can be set */
final class Var[T](initval: T) extends Signal[T] with Source[T] {
  pulses.current = Pulse.unchanged(initval)

  def update(value: T)(implicit fac: Engine): Unit = set(value)
  def set(value: T)(implicit fac: Engine): Unit = fac.plan { admit(value)(_) }

  def admit(value: T)(implicit turn: Turn): Unit =
    turn.admit(this) {
      val p = Pulse.diff(value, get)
      if (p.isChange) {
        pulses.set(p)
      }
    }

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult =
    EvaluationResult.Static(changed = pulse.isChange)
}

object Var {
  def apply[T](initval: T) = new Var(initval)
}

