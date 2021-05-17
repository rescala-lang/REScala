package rescala.operator

import rescala.core._
import rescala.interface.RescalaInterface

trait Sources {
  self : RescalaInterface =>

  trait Source[T] extends ReSource {
    final def admit(value: T)(implicit ticket: AdmissionTicket): Unit = admitPulse(Pulse.Value(value))
    def admitPulse(pulse: Pulse[T])(implicit ticket: AdmissionTicket): Unit
  }

  /** Source events with imperative occurrences
    *
    * @param initialState of by the event
    * @tparam T Type returned when the event fires
    * @tparam S Struct type used for the propagation of the event
    */
  class Evt[T] private[rescala] (initialState: State[Pulse[T]], name: ReName)
      extends Base[Pulse[T]](initialState, name)
      with Source[T]
      with Event[T] {
    override type Value = Pulse[T]

    override protected[rescala] def commit(base: Value): Value = Pulse.NoChange

    override def internalAccess(v: Pulse[T]): Pulse[T] = v

    /** Trigger the event */
    @deprecated("use .fire instead of apply", "0.21.0")
    def apply(value: T)(implicit fac: Scheduler): Unit          = fire(value)
    def fire()(implicit fac: Scheduler, ev: Unit =:= T): Unit   = fire(ev(()))(fac)
    def fire(value: T)(implicit fac: Scheduler): Unit           = fac.forceNewTransaction(this) { admit(value)(_) }
    override def disconnect()(implicit engine: Scheduler): Unit = ()
    def admitPulse(pulse: Pulse[T])(implicit ticket: AdmissionTicket): Unit = {
      ticket.recordChange(new InitialChange {
        override val source: Evt.this.type = Evt.this
        override def writeValue(base: Pulse[T], writeCallback: Pulse[T] => Unit): Boolean = {
          writeCallback(pulse); true
        }
      })
    }
  }

  /** Source signals with imperatively updates.
    *
    * @tparam A Type stored by the signal
    * @tparam S Struct type used for the propagation of the signal
    */
  trait Var[A] extends Source[A] with Signal[A] with Interp[A] {
    override type Value = Pulse[A]

    //def update(value: A)(implicit fac: Engine): Unit = set(value)
    def set(value: A)(implicit fac: Scheduler): Unit = fac.forceNewTransaction(this) { admit(value)(_) }

    def transform(f: A => A)(implicit fac: Scheduler): Unit =
      fac.forceNewTransaction(this) { t =>
        admit(f(t.now(this)))(t)
      }

    def setEmpty()(implicit fac: Scheduler): Unit = fac.forceNewTransaction(this)(t => admitPulse(Pulse.empty)(t))

    def admitPulse(pulse: Pulse[A])(implicit ticket: AdmissionTicket): Unit = {
      ticket.recordChange(new InitialChange {
        override val source: Var.this.type = Var.this
        override def writeValue(base: Pulse[A], writeCallback: Pulse[A] => Unit): Boolean =
          if (base != pulse) { writeCallback(pulse); true }
          else false
      })
    }
  }
}
