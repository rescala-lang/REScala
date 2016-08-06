package rescala.reactives

import rescala.engines.{Ticket, Engine}
import rescala.graph._
import rescala.propagation.Turn

import scala.language.higherKinds

sealed trait Source[T, S <: Struct] {
  def admit(value: T)(implicit turn: Turn[S]): Unit
}

/**
  * An implementation of an imperative event
  */
trait Evt[T, S <: Struct, SL[+X, Z <: Struct] <: Signal[X, Z, SL, EV], EV[+X, Z <: Struct] <: Event[X, Z, SL, EV]] extends Event[T, S, SL, EV] with Source[T, S] {
  /** Trigger the event */
  final def apply(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fire(value)
  final def fire()(implicit fac: Engine[S, Turn[S]], ev: Unit =:= T): Unit = fire(ev(Unit))(fac)
  def fire(value: T)(implicit fac: Engine[S, Turn[S]]): Unit
}

final class EvtImpl[T, S <: Struct]()(_bud: S#SporeP[T, Reactive[S]]) extends Base[T, S](_bud) with EventImpl[T, S] with Evt[T, S, SignalImpl, EventImpl] {
  override def fire(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) {admit(value)(_)}

  override def admit(value: T)(implicit turn: Turn[S]): Unit = {
    pulses.set(Pulse.change(value))(turn)
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = pulse.isChange)
}

object Evt {
  def apply[T, S <: Struct]()(implicit ticket: Ticket[S]): EvtImpl[T, S] = ticket { t => t.create(Set.empty)(new EvtImpl[T, S]()(t.bud(transient = true))) }
}

trait Var[T, S <: Struct, SL[+X, Z <: Struct] <: Signal[X, Z, SL, EV], EV[+X, Z <: Struct] <: Event[X, Z, SL, EV]] extends Signal[T, S, SL, EV] with Source[T, S] {
  final def update(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = set(value)
  def set(value: T)(implicit fac: Engine[S, Turn[S]]): Unit
  def transform(f: T => T)(implicit fac: Engine[S, Turn[S]]): Unit

  def admit(value: T)(implicit turn: Turn[S]): Unit
}

/** A root Reactive value without dependencies which can be set */
final class VarImpl[T, S <: Struct](initval: T)(_bud: S#SporeP[T, Reactive[S]]) extends Base[T, S](_bud) with SignalImpl[T, S] with Var[T, S, SignalImpl, EventImpl] {
  override def set(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) {admit(value)(_)}
  override def transform(f: T => T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) { t => admit(f(get(t)))(t) }

  override def admit(value: T)(implicit turn: Turn[S]): Unit = {
    val p = Pulse.diff(value, get)
    if (p.isChange) {
      pulses.set(p)
    }
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = pulse.isChange)
}

object Var {
  def apply[T, S <: Struct](initval: T)(implicit ticket: Ticket[S]): VarImpl[T, S] = ticket { t => t.create(Set.empty)(new VarImpl(initval)(t.bud(Pulse.unchanged(initval), transient = false))) }
}

