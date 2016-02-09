package rescala

import rescala.graph._
import rescala.turns.{Engine, Turn}

sealed trait Source[T, S <: Spores] {
  def admit(value: T)(implicit turn: Turn[S]): Unit
}

/**
 * An implementation of an imperative event
 */
final class Evt[T, S <: Spores]()(engine: S) extends Base[S](engine.bud()) with Event[T, S] with Source[T, S] {

  /** Trigger the event */
  def apply(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) { admit(value)(_) }

  def admit(value: T)(implicit turn: Turn[S]): Unit = {
    pulses.set(Pulse.change(value))(turn)
    turn.admit(this)
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = pulse.isChange)

  override protected[rescala] def incoming(implicit turn: Turn[S]): Set[Reactive[S]] = Set.empty
}

object Evt {
  def apply[T, S <: Spores]()(implicit engine: Engine[S, Turn[S]]): Evt[T, S] = new Evt[T, S]()(engine.bufferFactory)
}


/** A root Reactive value without dependencies which can be set */
final class Var[T, S <: Spores](initval: T)(engine: S) extends Base[S](engine.bud(Pulse.unchanged(initval), transient = false)) with Signal[T, S] with Source[T, S] {
  def update(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = set(value)
  def set(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) { admit(value)(_) }
  def transform(f: T => T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) { t => admit(f(get(t)))(t) }

  def admit(value: T)(implicit turn: Turn[S]): Unit = {
    val p = Pulse.diff(value, get)
    if (p.isChange) {
      pulses.set(p)
      turn.admit(this)
    }
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = pulse.isChange)

  override protected[rescala] def incoming(implicit turn: Turn[S]): Set[Reactive[S]] = Set.empty
}

object Var {
  def apply[T, S <: Spores](initval: T)(implicit engine: Engine[S, Turn[S]]): Var[T, S] = new Var(initval)(engine.bufferFactory)
}

