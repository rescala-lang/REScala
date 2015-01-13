package rescala

import rescala.graph.{Reactive, Enlock, ReevaluationResult, Pulse}
import rescala.turns.{Engine, Turn}

sealed trait Source[T] {
  def admit(value: T)(implicit turn: Turn): Unit
}

/**
 * An implementation of an imperative event
 */
final class Evt[T]()(engine: Engine[Turn]) extends Enlock(engine) with Event[T] with Source[T] {

  /** Trigger the event */
  def apply(value: T)(implicit fac: Engine[Turn]): Unit = fac.plan(this) { admit(value)(_) }

  def admit(value: T)(implicit turn: Turn): Unit = {
    pulses.set(Pulse.change(value))(turn)
    turn.admit(this)
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn): ReevaluationResult =
    ReevaluationResult.Static(changed = pulse.isChange)

  override protected[rescala] def incoming(implicit turn: Turn): Set[Reactive] = Set.empty
}

object Evt {
  def apply[T]()(implicit engine: Engine[Turn]) = new Evt[T]()(engine)
}


/** A root Reactive value without dependencies which can be set */
final class Var[T](initval: T)(engine: Engine[Turn]) extends Enlock(engine) with Signal[T] with Source[T] {
  pulses.initCurrent(Pulse.unchanged(initval))

  def update(value: T)(implicit fac: Engine[Turn]): Unit = set(value)
  def set(value: T)(implicit fac: Engine[Turn]): Unit = fac.plan(this) { admit(value)(_) }

  def admit(value: T)(implicit turn: Turn): Unit = {
    val p = Pulse.diff(value, get)
    if (p.isChange) {
      pulses.set(p)
      turn.admit(this)
    }
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn): ReevaluationResult =
    ReevaluationResult.Static(changed = pulse.isChange)

  override protected[rescala] def incoming(implicit turn: Turn): Set[Reactive] = Set.empty
}

object Var {
  def apply[T](initval: T)(implicit engine: Engine[Turn]) = new Var(initval)(engine)
}

