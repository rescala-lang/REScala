package rescala.reactives

import rescala.core._
import rescala.reactives.Events.Estate

abstract class Source[S <: Struct, T](name: REName) extends RENamed(name) with ReSource[S] {

  final def admit(value: T)(implicit ticket: AdmissionTicket[S]): Unit = admitPulse(Pulse.Value(value))
  def admitPulse(pulse: Pulse[T])(implicit ticket: AdmissionTicket[S]): Unit
}

/** Source events with imperative occurrences
  *
  * @param initialState of by the event
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
final class Evt[T, S <: Struct] private[rescala](initialState: Estate[S, T], name: REName)
  extends Source[S, T](name) with Event[T, S] {
  override type Notification = Pulse[T]
  override type Value = Nothing
  override protected[rescala] def state: S#State[Nothing, S, Pulse[T]] = initialState

  /** Trigger the event */
  @deprecated("use .fire instead of apply", "0.21.0")
  def apply(value: T)(implicit fac: Scheduler[S]): Unit = fire(value)
  def fire()(implicit fac: Scheduler[S], ev: Unit =:= T): Unit = fire(ev(Unit))(fac)
  def fire(value: T)(implicit fac: Scheduler[S]): Unit = fac.transaction(this) {admit(value)(_)}
  override def disconnect()(implicit engine: Scheduler[S]): Unit = ()
  def admitPulse(pulse: Pulse[T])(implicit ticket: AdmissionTicket[S]): Unit = {
    ticket.recordChange(new InitialChangeN[S] {
      override val source = Evt.this
      override val value: source.Notification = pulse
    })
  }
}

/** Creates new [[Evt]]s */
object Evt {
  def apply[T, S <: Struct]()(implicit ticket: CreationTicket[S]): Evt[T, S] = ticket { t =>
    t.createSource[Nothing, Evt[T, S], Pulse[T]](Initializer.Event)(new Evt[T, S](_, ticket.rename))
  }
}

/** Source signals with imperatively updates.
  *
  * @param initialState of the signal
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
final class Var[A, S <: Struct] private[rescala](initialState: Signals.Sstate[S, A], name: REName)
  extends Source[S, A](name) with Signal[A, S] {
  override type Value = Pulse[A]
  override type Notification = Nothing

  override protected[rescala] def state: S#State[Pulse[A], S, Nothing] = initialState

  //def update(value: A)(implicit fac: Engine[S]): Unit = set(value)
  def set(value: A)(implicit fac: Scheduler[S]): Unit = fac.transaction(this) {admit(value)(_)}

  def transform(f: A => A)(implicit fac: Scheduler[S]): Unit = fac.transaction(this) { t =>
    admit(f(t.now(this)))(t)
  }

  def setEmpty()(implicit fac: Scheduler[S]): Unit = fac.transaction(this)(t => admitPulse(Pulse.empty)(t))

  override def disconnect()(implicit engine: Scheduler[S]): Unit = ()

  final def admitPulse(pulse: Pulse[A])(implicit ticket: AdmissionTicket[S]): Unit = {
    ticket.recordChange(new InitialChangeV[S] {
      override val source: Var.this.type = Var.this
      override val value: Value = pulse
      override def accept(before: source.Value): Boolean = before != value
    })
  }
}

/** Creates new [[Var]]s */
object Var {
  def apply[T: ReSerializable, S <: Struct](initval: T)(implicit ticket: CreationTicket[S]): Var[T, S] = fromChange(Pulse.Value(initval))
  def empty[T: ReSerializable, S <: Struct]()(implicit ticket: CreationTicket[S]): Var[T, S] = fromChange(Pulse.empty)
  private[this] def fromChange[T: ReSerializable, S <: Struct](change: Pulse.Change[T])(implicit ticket: CreationTicket[S]): Var[T, S] = ticket { t =>
    t.createSource[Pulse[T], Var[T, S], Nothing](Initializer.InitializedSignal(change))(new Var[T, S](_, ticket.rename))
  }
}

