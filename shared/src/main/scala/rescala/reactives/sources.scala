package rescala.reactives

import rescala.engines.{Engine, Ticket}
import rescala.graph._
import rescala.propagation.Turn
import rescala.reactives.RExceptions.EmptySignalControlThrowable

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

/**
  * Interface for source reactives with no dependencies that can be manually written by the user.
  *
  * @tparam T Type of the source reactive
  * @tparam S Struct type used for the propagation of the reactive
  */
sealed trait Source[T, S <: Struct] {
  def admit(value: T)(implicit turn: Turn[S]): Unit
}

/**
  * Inferface for source events with no dependencies that can be manually fired by the user.
  *
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  * @tparam SL Signal type supported as parameter and used as return type for event methods
  * @tparam EV Event type supported as parameter and used as return type for event methods
  */
trait EvtLike[T, S <: Struct, SL[+X, Z <: Struct] <: SignalLike[X, Z, SL, EV], EV[+X, Z <: Struct] <: EventLike[X, Z, SL, EV]] extends EventLike[T, S, SL, EV] with Source[T, S] {
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

  def fireFromTry(value: Try[T])(implicit fac: Engine[S, Turn[S]]): Unit = value match {
    case Success(suc) => fire(suc)
    case Failure(f) => fac.plan(this)(t => pulses(t).set(Pulse.Exceptional(f))(t))
  }

 override def admit(value: T)(implicit turn: Turn[S]): Unit = {
    pulses.set(Pulse.Change(value))(turn)
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = pulse.isChange)
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
trait VarLike[A, S <: Struct, SL[+X, Z <: Struct] <: SignalLike[X, Z, SL, EV], EV[+X, Z <: Struct] <: EventLike[X, Z, SL, EV]] extends SignalLike[A, S, SL, EV] with Source[A, S] {
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
  def setFromTry(value: Try[A])(implicit fac: Engine[S, Turn[S]]): Unit = value match {
    case Success(suc) => set(suc)
    case Failure(f) => fac.plan(this)(t => pulses(t).set(Pulse.Exceptional(f))(t))
  }

  override def admit(value: A)(implicit turn: Turn[S]): Unit = {
    val p = Pulse.diffPulse(value, stable)
    if (p.isChange) {
      pulses.set(p)
    }
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] =
    ReevaluationResult.Static(changed = pulse.isChange)
}

/**
  * Companion object that allows external users to create new source signals.
  */
object Var {
  def apply[T, S <: Struct](initval: T)(implicit ticket: Ticket[S]): Var[T, S] = ticket { t => t.create(Set.empty)(new Var(t.bud(Pulse.Stable(initval), transient = false))) }
  def empty[T, S <: Struct]()(implicit ticket: Ticket[S]): Var[T, S] = ticket { t => t.create(Set.empty)(new Var(t.bud(Pulse.Exceptional(new EmptySignalControlThrowable), transient = false))) }

}

