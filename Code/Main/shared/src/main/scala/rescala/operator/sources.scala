package rescala.operator

import rescala.core._
import rescala.interface.RescalaInterface

trait Sources {
  self: RescalaInterface with EventBundle with SignalBundle with Sources with DefaultImplementations with Observing
    with Core =>

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
    def apply(value: T)(implicit fac: Scheduler): Unit        = fire(value)(fac)
    def fire()(implicit fac: Scheduler, ev: Unit =:= T): Unit = fire(ev(()))(fac)
    def fire(value: T)(implicit fac: Scheduler): Unit         = fac.forceNewTransaction(this) { admit(value)(_) }
    override def disconnect(): Unit                           = ()
    def admitPulse(pulse: Pulse[T])(implicit ticket: AdmissionTicket): Unit = {
      ticket.recordChange(new InitialChange {
        override val source: Evt.this.type = Evt.this
        override def writeValue(base: Pulse[T], writeCallback: Pulse[T] => Unit): Boolean = {
          writeCallback(pulse); true
        }
      })
    }
  }

  /** @group create */
  final def Evt[A]()(implicit ticket: CreationTicket): Evt[A] = {
    ticket.createSource[Pulse[A], Evt[A]](Pulse.NoChange)(init => { new Evt[A](init, ticket.rename) }: Evt[A])
  }

  /** Source signals with imperatively updates.
    *
    * @tparam A Type stored by the signal
    * @tparam S Struct type used for the propagation of the signal
    */
  class Var[A] private[rescala] (initialState: State[Pulse[A]], name: ReName) extends Base[Pulse[A]](initialState, name)
      with Source[A] with Signal[A] with Readable[A] {
    override type Value = Pulse[A]

    override val resource: Signal[A] = this
    override def disconnect(): Unit  = ()

    // def update(value: A)(implicit fac: Engine): Unit = set(value)
    def set(value: A)(implicit fac: Scheduler): Unit = fac.forceNewTransaction(this) { admit(value)(_) }

    def transform(f: A => A)(implicit fac: Scheduler): Unit =
      fac.forceNewTransaction(this) { t =>
        admit(f(t.tx.now(this)))(t)
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

  /** Creates new [[Var]]s
    * @group create
    */
  object Var {
    def apply[T](initval: T)(implicit ticket: CreationTicket): Var[T] = fromChange(Pulse.Value(initval))
    def empty[T](implicit ticket: CreationTicket): Var[T]             = fromChange(Pulse.empty)
    private[this] def fromChange[T](change: Pulse[T])(implicit ticket: CreationTicket): Var[T] = {
      ticket.createSource[Pulse[T], Var[T]](change)(s => new Var[T](s, ticket.rename))
    }
  }

}
