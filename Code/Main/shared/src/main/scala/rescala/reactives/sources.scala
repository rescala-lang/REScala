package rescala.reactives

import rescala.core._
import rescala.interface.RescalaInterface
import rescala.reactives.Events.Estate



trait Source[S <: Struct, T] extends ReSource[S] {
  final def admit(value: T)(implicit ticket: AdmissionTicket[S]): Unit = admitPulse(Pulse.Value(value))
  def admitPulse(pulse: Pulse[T])(implicit ticket: AdmissionTicket[S]): Unit
}

/** Source events with imperative occurrences
  *
  * @param initialState of by the event
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
abstract class Evt[T, S <: Struct] private[rescala](initialState: Estate[S, T], name: REName)
  extends Base[Pulse[T], S](initialState, name) with Source[S, T] with Event[T, S] {
  override type Value = Pulse[T]

  override def internalAccess(v: Pulse[T]): Pulse[T] = v
  /** Trigger the event */
  @deprecated("use .fire instead of apply", "0.21.0")
  def apply(value: T)(implicit fac: Scheduler[S]): Unit = fire(value)
  def fire()(implicit fac: Scheduler[S], ev: Unit =:= T): Unit = fire(ev(()))(fac)
  def fire(value: T)(implicit fac: Scheduler[S]): Unit = fac.forceNewTransaction(this) {admit(value)(_)}
  override def disconnect()(implicit engine: Scheduler[S]): Unit = ()
  def admitPulse(pulse: Pulse[T])(implicit ticket: AdmissionTicket[S]): Unit = {
    ticket.recordChange(new InitialChange[S] {
      override val source = Evt.this
      override def writeValue(b: Pulse[T], v: Pulse[T] => Unit): Boolean = {v(pulse); true}
    })
  }
}

///** Creates new [[Evt]]s */
//object Evt {
//  def apply[T, S <: Struct]()(implicit ticket: CreationTicket[S]): Evt[T, S] =
//    ticket.createSource[Pulse[T], Evt[T, S]](Initializer.Event)(new Evt[T, S](_, ticket.rename))
//}


/** Source signals with imperatively updates.
  *
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
trait Var[A, S <: Struct] extends Source[S, A] with Signal[A, S] with Interp[A, S] {
  override type Value = Pulse[A]

  //def update(value: A)(implicit fac: Engine[S]): Unit = set(value)
  def set(value: A)(implicit fac: Scheduler[S]): Unit = fac.forceNewTransaction(this) {admit(value)(_)}

  def transform(f: A => A)(implicit fac: Scheduler[S]): Unit = fac.forceNewTransaction(this) { t =>
    admit(f(t.now(this)))(t)
  }

  def setEmpty()(implicit fac: Scheduler[S]): Unit = fac.forceNewTransaction(this)(t => admitPulse(Pulse.empty)(t))

  def admitPulse(pulse: Pulse[A])(implicit ticket: AdmissionTicket[S]): Unit = {
    ticket.recordChange(new InitialChange[S] {
      override val source: Var.this.type = Var.this
      override def writeValue(b: Pulse[A], v: Pulse[A] => Unit): Boolean = if (b != pulse) {v(pulse); true} else false
    })
  }
}
