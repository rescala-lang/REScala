package rescala.reactives

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

import rescala.core.{REName, Reactive, _}

abstract class Source[T, S <: Struct](initialState: S#State[Pulse[T], S], name: REName) extends Base[T, S](initialState, name) {
  // TODO this should be stored on the admission ticket
  private val buffer: ConcurrentMap[Turn[S], Pulse[T]] = new ConcurrentHashMap()
  final def admit(value: T)(implicit ticket: AdmissionTicket[S]): Unit = admitPulse(Pulse.Value(value))

  final def admitPulse(value: Pulse[T])(implicit ticket: AdmissionTicket[S]): Unit = {
    require(value != null, s"cannot admit null")
    val turn: Turn[S] = ticket.creation.asInstanceOf[Turn[S]]
    val wasSetBefore = buffer.putIfAbsent(turn, value) != null
    require(!wasSetBefore, s"cannot admit the same reactive twice in the same turn: ${ticket.creation}")
  }

  final override protected[rescala] def reevaluate(turn: Turn[S], before: Pulse[T], indeps: Set[Reactive[S]]): ReevaluationResult[Value, S] = {
    val value = buffer.remove(turn)
    ReevaluationResult.Static(if(value == null) Pulse.NoChange else pulseFromBufferedPulse(turn, before, value), indeps)
  }

  protected def pulseFromBufferedPulse(turn: Turn[S], before: Pulse[T], value: Pulse[T]): Pulse[T]
}

/**
  * Source events that can be imperatively updated
  *
  * @param initialState of by the event
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
final class Evt[T, S <: Struct] private[rescala] (initialState: S#State[Pulse[T], S], name: REName) extends Source[T, S](initialState, name) with Event[T, S] {
  /** Trigger the event */
  def apply(value: T)(implicit fac: Engine[S]): Unit = fire(value)
  def fire()(implicit fac: Engine[S], ev: Unit =:= T): Unit = fire(ev(Unit))(fac)
  def fire(value: T)(implicit fac: Engine[S]): Unit = fac.transaction(this) {admit(value)(_)}
  override def disconnect()(implicit engine: Engine[S]): Unit = ()

  override protected def pulseFromBufferedPulse(turn: Turn[S], before: Pulse[T], value: Pulse[T]): Pulse[T] = value
}

/**
  * Companion object that allows external users to create new source events.
  */
object Evt {
  def apply[T, S <: Struct]()(implicit ticket: CreationTicket[S]): Evt[T, S] = ticket { t =>
    t.create[Pulse[T], Evt[T, S]](Set.empty, ValuePersistency.Event)(new Evt[T, S](_, ticket.rename))
  }
}

/**
  * Source signals that can be imperatively updated
  *
  * @param initialState of the signal
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
final class Var[A, S <: Struct] private[rescala] (initialState: S#State[Pulse[A], S], name: REName) extends Source[A, S](initialState, name) with Signal[A, S] {
  def update(value: A)(implicit fac: Engine[S]): Unit = set(value)
  def set(value: A)(implicit fac: Engine[S]): Unit = fac.transaction(this) {admit(value)(_)}

  def transform(f: A => A)(implicit fac: Engine[S]): Unit = fac.transaction(this) { t =>
    // minor TODO should f be evaluated only during reevaluation, given t.selfBefore(this) as parameter?
    admit(f(t.now(this)))(t)
  }

  override protected def pulseFromBufferedPulse(turn: Turn[S], before: Pulse[A], value: Pulse[A]): Pulse[A] = if(value == before) Pulse.NoChange else value

  def setEmpty()(implicit fac: Engine[S]): Unit = fac.transaction(this)(t => admitPulse(Pulse.empty)(t))

  override def disconnect()(implicit engine: Engine[S]): Unit = ()
}

/**
  * Companion object that allows external users to create new source signals.
  */
object Var {
  def apply[T: ReSerializable, S <: Struct](initval: T)(implicit ticket: CreationTicket[S]): Var[T, S] = fromChange(Pulse.Value(initval))
  def empty[T: ReSerializable, S <: Struct]()(implicit ticket: CreationTicket[S]): Var[T, S] = fromChange(Pulse.empty)
  private[this] def fromChange[T: ReSerializable, S <: Struct](change: Pulse.Change[T])(implicit ticket: CreationTicket[S]): Var[T, S] = ticket { t =>
    t.create[Pulse[T], Var[T, S]](Set.empty, ValuePersistency.InitializedSignal(change))(new Var[T, S](_, ticket.rename))
  }
}

