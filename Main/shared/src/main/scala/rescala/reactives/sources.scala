package rescala.reactives

import rescala.engine.{Engine, Turn, TurnSource}
import rescala.graph._

import scala.language.higherKinds

class Source[T, S <: Struct](_bud: S#State[Pulse[T], S]) extends Base[T, S](_bud) {
  private var result: Value = null
  final def admit(value: T)(implicit turn: Turn[S]): Unit = admitPulse(Pulse.Change(value))

  final def admitPulse(value: Pulse[T])(implicit turn: Turn[S]): Unit = {
    require(result == null, "can not admit the same reactive twice in the same turn")
    result = value
  }

  final override protected[rescala] def reevaluate(turn: Turn[S]): ReevaluationResult[Value, S] = {
    val res: ReevaluationResult[Pulse[T], S] = if (result == null || result == turn.before(this))
      ReevaluationResult.Static(Pulse.NoChange)
    else ReevaluationResult.Static[T](result)
    result = null
    res
  }

}

/**
  * Standard implementation of the source event interface using Spore-based propagation.
  *
  * @param _bud Spore used by the event
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
final class Evt[T, S <: Struct]()(_bud: S#State[Pulse[T], S]) extends Source[T, S](_bud) with Event[T, S] {
  /** Trigger the event */
  def apply(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fire(value)
  def fire()(implicit fac: Engine[S, Turn[S]], ev: Unit =:= T): Unit = fire(ev(Unit))(fac)
  def fire(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.transaction(this) {admit(value)(_)}
  override def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit = ()
}

/**
  * Companion object that allows external users to create new source events.
  */
object Evt {
  def apply[T, S <: Struct]()(implicit ticket: TurnSource[S]): Evt[T, S] = ticket { t => t.create(Set.empty)(new Evt[T, S]()(t.makeStructState(Pulse.NoChange, transient = true))) }
}

/**
  * Standard implementation of the source signal interface using Spore-based propagation.
  *
  * @param _bud Spore used by the signal
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
final class Var[A, S <: Struct](_bud: S#State[Pulse[A], S]) extends Source[A, S](_bud) with Signal[A, S] {
  def update(value: A)(implicit fac: Engine[S, Turn[S]]): Unit = set(value)
  def set(value: A)(implicit fac: Engine[S, Turn[S]]): Unit = fac.transaction(this) {admit(value)(_)}

  def transform(f: A => A)(implicit fac: Engine[S, Turn[S]]): Unit = fac.transaction(this) { t =>
    admit(f(t.before(this).get))(t) }

  def setEmpty()(implicit fac: Engine[S, Turn[S]]): Unit = fac.transaction(this)(t => admitPulse(Pulse.empty)(t))

  override def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit = ()
}

/**
  * Companion object that allows external users to create new source signals.
  */
object Var {
  def apply[T, S <: Struct](initval: T)(implicit ticket: TurnSource[S]): Var[T, S] = ticket { t => t.create(Set.empty)(new Var(t.makeStructState(Pulse.Change(initval), transient = false, hasState = true))) }
  def empty[T, S <: Struct]()(implicit ticket: TurnSource[S]): Var[T, S] = ticket { t => t.create(Set.empty)(new Var(t.makeStructState(Pulse.empty, transient = false, hasState = true))) }

}

