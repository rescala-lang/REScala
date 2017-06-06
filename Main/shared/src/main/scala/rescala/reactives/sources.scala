package rescala.reactives

import rescala.engine._
import rescala.graph._

class Source[T, S <: Struct](_bud: S#State[Pulse[T], S]) extends Base[T, S](_bud) {
  private var nextReevaluationResult: Value = null
  final def admit(value: T)(implicit ticket: AdmissionTicket[S]): Unit = admitPulse(Pulse.Value(value))

  final def admitPulse(value: Pulse[T])(implicit ticket: AdmissionTicket[S]): Unit = {
    require(nextReevaluationResult == null, s"can not admit the same reactive twice in the same turn: ${ticket.turn}")
    nextReevaluationResult = value
  }

  override protected[rescala] def reevaluate(turn: Turn[S]): ReevaluationResult.Static[Value] = {
    val value = nextReevaluationResult
    nextReevaluationResult = null
    if (value == null) ReevaluationResult.staticNoChange
    else ReevaluationResult.Static[T](value)
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
  def apply[T, S <: Struct]()(implicit ticket: TurnSource[S]): Evt[T, S] = ticket { t =>
    t.create[Pulse[T], Evt[T, S]](Set.empty, ValuePersistency.Event)(new Evt[T, S]()(_))
  }
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
    // minor TODO should f be evaluated only during reevaluation, given t.selfBefore(this) as parameter?
    admit(f(t.before(this).get))(t)
  }

  override protected[rescala] def reevaluate(turn: Turn[S]): ReevaluationResult.Static[Pulse[A]] = {
    val res = super.reevaluate(turn)
    if (res.value == turn.selfBefore(this)) ReevaluationResult.staticNoChange
    else res
  }

  def setEmpty()(implicit fac: Engine[S, Turn[S]]): Unit = fac.transaction(this)(t => admitPulse(Pulse.empty)(t))

  override def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit = ()
}

/**
  * Companion object that allows external users to create new source signals.
  */
object Var {
  def apply[T, S <: Struct](initval: T)(implicit ticket: TurnSource[S]): Var[T, S] = fromChange(Pulse.Value(initval))
  def empty[T, S <: Struct]()(implicit ticket: TurnSource[S]): Var[T, S] = fromChange(Pulse.empty)
  private[this] def fromChange[T, S <: Struct](change: Change[T])(implicit ticket: TurnSource[S]): Var[T, S] = ticket { t =>
    t.create[Pulse[T], Var[T, S]](Set.empty, ValuePersistency.InitializedSignal(change))(new Var[T, S](_))
  }
}

