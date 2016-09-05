package rescala.reactives

import rescala.engines.{Engine, Ticket}
import rescala.graph._
import rescala.propagation.Turn

import scala.language.higherKinds

/**
  * Inferface for source events with no dependencies that can be manually fired by the user.
  *
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  * @tparam SL Signal type supported as parameter and used as return type for event methods
  * @tparam EV Event type supported as parameter and used as return type for event methods
  */
trait EvtLike[T, S <: Struct, SL[+X, Z <: Struct] <: SignalLike[X, Z, SL, EV], EV[+X, Z <: Struct] <: EventLike[X, Z, SL, EV]] extends EventLike[T, S, SL, EV]  {
  this : EV[T, S] =>
  /** Trigger the event */
  final def apply(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fire(value)
  final def fire()(implicit fac: Engine[S, Turn[S]], ev: Unit =:= T): Unit = fire(ev(Unit))(fac)
  def fire(value: T)(implicit fac: Engine[S, Turn[S]]): Unit
}

/**
  * Standard implementation of the source event interface using Spore-based propagation.
  *
  * @param _bud Spore used by the event
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
final class Evt[T, S <: Struct]()(_bud: S#SporeP[T, Reactive[S]]) extends Base[T, S](_bud) with Event[T, S] with EvtLike[T, S, Signal, Event] {
  override def fire(value: T)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) {admit(value)(_)}

  def admit(value: T)(implicit turn: Turn[S]): Unit = admitPulse(Pulse.Change(value))

  def admitPulse(value: Pulse[T])(implicit turn: Turn[S]): Unit = {
    require(!pulse.isChange, "can not admit the same reactive twice in the same turn")
    pulses.set(value)(turn)
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = pulse.isChange)

  override def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit = ()
}

/**
  * Companion object that allows external users to create new source events.
  */
object Evt {
  def apply[T, S <: Struct]()(implicit ticket: Ticket[S]): Evt[T, S] = ticket { t => t.create(Set.empty)(new Evt[T, S]()(t.bud(transient = true))) }
}

/**
  * Interface for source signals with no dependencies that can be manually set by the user.
  *
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  * @tparam SL Signal type supported as parameter and used as return type for signal methods
  * @tparam EV Event type supported as parameter and used as return type for signal methods
  */
trait VarLike[A, S <: Struct, SL[+X, Z <: Struct] <: SignalLike[X, Z, SL, EV], EV[+X, Z <: Struct] <: EventLike[X, Z, SL, EV]] extends SignalLike[A, S, SL, EV]  {
  this : SL[A, S] =>

  def update(value: A)(implicit fac: Engine[S, Turn[S]]): Unit = set(value)
  def set(value: A)(implicit fac: Engine[S, Turn[S]]): Unit
  def transform(f: A => A)(implicit fac: Engine[S, Turn[S]]): Unit

  def admit(value: A)(implicit turn: Turn[S]): Unit
}


/**
  * Standard implementation of the source signal interface using Spore-based propagation.
  *
  * @param _bud Spore used by the signal
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
final class Var[A, S <: Struct](_bud: S#SporeP[A, Reactive[S]]) extends Base[A, S](_bud) with Signal[A, S] with VarLike[A, S, Signal, Event] {

  override def set(value: A)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) {admit(value)(_)}

  override def transform(f: A => A)(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this) { t => admit(f(get(t)))(t) }

  def setEmpty()(implicit fac: Engine[S, Turn[S]]): Unit = fac.plan(this)(t => admitPulse(Pulse.empty)(t))

  def admit(value: A)(implicit turn: Turn[S]): Unit = admitPulse(Pulse.diffPulse(value, stable))

  def admitPulse(p: Pulse[A])(implicit turn: Turn[S]): Unit = {
    require(pulse == stable, "can not admit the same reactive twice in the same turn")
    if (p.isChange) { pulses.set(p) }
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = pulse.isChange)

  override def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit = ()
}

/**
  * Companion object that allows external users to create new source signals.
  */
object Var {
  def apply[T, S <: Struct](initval: T)(implicit ticket: Ticket[S]): Var[T, S] = ticket { t => t.create(Set.empty)(new Var(t.bud(Pulse.Stable(initval), transient = false))) }
  def empty[T, S <: Struct]()(implicit ticket: Ticket[S]): Var[T, S] = ticket { t => t.create(Set.empty)(new Var(t.bud(Pulse.empty, transient = false))) }

}

