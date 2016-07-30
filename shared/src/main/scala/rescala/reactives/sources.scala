package rescala.reactives

import rescala.engines.{Ticket, Engine}
import rescala.graph._
import rescala.propagation.Turn

sealed trait Source[T, S <: Struct] {
  def admit(value: T)(implicit turn: Turn[S]): Unit
}

/**
  * An implementation of an imperative event
  */
final class Evt[T, S <: Struct]()(_bud: S#SporeP[T, Reactive[S]]) extends Base[T, S](_bud) with EventImpl[T, S] with Source[T, S] {

  /** Trigger the event */
  def apply(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fire(value)
  def fire()(implicit fac: Engine[S, Turn[S]], ev: Unit =:= T): Unit = fire(ev(Unit))(fac)
  def fire(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) {admit(value)(_)}



  def admit(value: T)(implicit turn: Turn[S]): Unit = {
    pulses.set(Pulse.change(value))(turn)
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = pulse.isChange)
}

object Evt {
  def apply[T, S <: Struct]()(implicit ticket: Ticket[S]): Evt[T, S] = ticket { t => t.create(Set.empty)(new Evt[T, S]()(t.bud(transient = true))) }
}


/** A root Reactive value without dependencies which can be set */
final class Var[T, S <: Struct](initval: T)(_bud: S#SporeP[T, Reactive[S]]) extends Base[T, S](_bud) with SignalImpl[T, S] with Source[T, S] {
  def update(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = set(value)
  def set(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) {admit(value)(_)}
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
  def apply[T, S <: Struct](initval: T)(implicit ticket: Ticket[S]): Var[T, S] = ticket { t => t.create(Set.empty)(new Var(initval)(t.bud(Pulse.unchanged(initval), transient = false))) }
}

