package rescala.reactives

import rescala.engines.Engine
import rescala.graph._
import rescala.propagation.Turn

sealed trait Source[T, S <: Struct] {
  def admit(value: T)(implicit turn: Turn[S]): Unit
}

/**
 * An implementation of an imperative event
 */
final class Evt[T, S <: Struct]()(engine: S) extends Base[T, S](engine.bud()) with Event[T, S] with Source[T, S] {

  /** Trigger the event */
  def apply(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) { admit(value)(_) }

  def admit(value: T)(implicit turn: Turn[S]): Unit = {
    pulses.set(Pulse.change(value))(turn)
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = pulse.isChange)
}

object Evt {
  def apply[T, S <: Struct]()(implicit engine: Engine[S, Turn[S]]): Evt[T, S] = new Evt[T, S]()(engine.bufferFactory)
}


/** A root Reactive value without dependencies which can be set */
final class Var[T, S <: Struct](initval: T)(engine: S) extends Base[T, S](engine.bud(Pulse.unchanged(initval), transient = false)) with Signal[T, S] with Source[T, S] {
  def update(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = set(value)
  def set(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) { admit(value)(_) }
  def transform(f: T => T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) { t => admit(f(get(t)))(t) }

  def admit(value: T)(implicit turn: Turn[S]): Unit = {
    val p = Pulse.diff(value, get)
    if (p.isChange) {
      pulses.set(p)
    }
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = pulse.isChange)
}

object Var {
  def apply[T, S <: Struct](initval: T)(implicit engine: Engine[S, Turn[S]]): Var[T, S] = new Var(initval)(engine.bufferFactory)
}

