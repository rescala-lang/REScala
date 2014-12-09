package rescala

import rescala.turns.{Turn, Engine}
import rescala.graph.{EvaluationResult, Pulse}


/**
 * An implementation of an imperative event
 */
final class Evt[T]() extends Event[T] {

  /** Trigger the event */
  def apply(v: T)(implicit fac: Engine): Unit = fac.startNew { turn =>
    turn.admit(this) {
      pulses.set(Pulse.change(v))(turn)
      true
    }
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult =
    EvaluationResult.Static(changed = true)
}

object Evt {
  def apply[T]() = new Evt[T]()
}



/** A root Reactive value without dependencies which can be set */
final class Var[T](initval: T) extends Signal[T] {
  pulses.current = Pulse.unchanged(initval)

  def update(newValue: T)(implicit fac: Engine): Unit = set(newValue)
  def set(newValue: T)(implicit fac: Engine): Unit = fac.startNew { turn =>
    admit(newValue)(turn)
  }

  def admit(newValue: T)(implicit turn: Turn): Unit =
    turn.admit(this) {
      val p = Pulse.diff(newValue, get)
      if (p.isChange) {
        pulses.set(p)
        true
      }
      else false
    }

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult =
    EvaluationResult.Static(changed = true)
}

object Var {
  def apply[T](initval: T) = new Var(initval)
}

