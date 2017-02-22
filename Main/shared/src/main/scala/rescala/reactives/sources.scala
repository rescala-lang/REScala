package rescala.reactives

import rescala.engine.{Engine, TurnSource}
import rescala.graph._
import rescala.propagation.Turn

import scala.language.higherKinds

/**
  * Standard implementation of the source event interface using Spore-based propagation.
  *
  * @param _bud Spore used by the event
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
final class Evt[T, S <: Struct]()(_bud: S#StructType[T, Reactive[S]]) extends Base[T, S](_bud) with Event[T, S] {
  /** Trigger the event */
  def apply(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fire(value)
  def fire()(implicit fac: Engine[S, Turn[S]], ev: Unit =:= T): Unit = fire(ev(Unit))(fac)
  def fire(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) {admit(value)(_)}

  def admit(value: T)(implicit turn: Turn[S]): Unit = admitPulse(Pulse.Change(value))

  def admitPulse(value: Pulse[T])(implicit turn: Turn[S]): Unit = {
    require(!hasChanged, "can not admit the same reactive twice in the same turn")
    set(value)(turn)
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = hasChanged)

  override def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit = ()
}

/**
  * Companion object that allows external users to create new source events.
  */
object Evt {
  def apply[T, S <: Struct]()(implicit ticket: TurnSource[S]): Evt[T, S] = ticket { t => t.create(Set.empty)(new Evt[T, S]()(t.makeStructState(transient = true))) }
}

/**
  * Standard implementation of the source signal interface using Spore-based propagation.
  *
  * @param _bud Spore used by the signal
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
final class Var[A, S <: Struct](_bud: S#StructType[A, Reactive[S]]) extends Base[A, S](_bud) with Signal[A, S] {

  def update(value: A)(implicit fac: Engine[S, Turn[S]]): Unit = set(value)
  def set(value: A)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) {admit(value)(_)}

  def transform(f: A => A)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) { t => admit(f(regRead(t)))(t) }

  def setEmpty()(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this)(t => admitPulse(Pulse.empty)(t))

  def admit(value: A)(implicit turn: Turn[S]): Unit = admitPulse(Pulse.diffPulse(value, stable))

  def admitPulse(p: Pulse[A])(implicit turn: Turn[S]): Unit = {
    require(!hasChanged, "can not admit the same reactive twice in the same turn")
    if (p.isChange) { set(p) }
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = hasChanged)

  override def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit = ()
}

/**
  * Companion object that allows external users to create new source signals.
  */
object Var {
  def apply[T, S <: Struct](initval: T)(implicit ticket: TurnSource[S]): Var[T, S] = ticket { t => t.create(Set.empty)(new Var(t.makeStructState(Pulse.Change(initval), transient = false))) }
  def empty[T, S <: Struct]()(implicit ticket: TurnSource[S]): Var[T, S] = ticket { t => t.create(Set.empty)(new Var(t.makeStructState(Pulse.empty, transient = false))) }

}

