package reactives.operator

import reactives.core.{
  AdmissionTicket, Base, CreationTicket, InitialChange, Observation, PlanTransactionScope, ReInfo, ReSource, Scheduler
}
import reactives.operator.Interface.State
import reactives.structure.Pulse

trait Source[T] extends reactives.core.ReSource {
  final def admit(value: T)(implicit ticket: AdmissionTicket[State]): Unit = admitPulse(Pulse.Value(value))
  def admitPulse(pulse: Pulse[T])(implicit ticket: AdmissionTicket[State]): Unit
}

/** Source events with imperative occurrences
  *
  * @param initialState of by the event
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
class Evt[T] private[reactives] (initialState: State[Pulse[T]], name: ReInfo)
    extends Base[Pulse[T]](initialState, name)
    with Source[T]
    with Event[T] {
  override type Value = Pulse[T]

  override protected[reactives] def commit(base: Value): Value = Pulse.NoChange

  override def internalAccess(v: Pulse[T]): Pulse[T] = v

  /** Trigger the event */
  @deprecated("use .fire instead of apply", "0.21.0")
  def apply(value: T)(using PlanTransactionScope[State]): Unit                = fire(value)
  infix def fire()(using PlanTransactionScope[State])(using Unit =:= T): Unit = fire(())
  infix def fire(value: T)(implicit planTransactionScope: PlanTransactionScope[State]): Unit =
    planTransactionScope.planTransaction(this)(admit(value)(_))
  override def disconnect(): Unit = ()
  def admitPulse(pulse: Pulse[T])(implicit ticket: AdmissionTicket[State]): Unit = {
    ticket.recordChange(new InitialChange[State] {
      override val source: Evt.this.type = Evt.this
      override def writeValue(base: Pulse[T], writeCallback: Pulse[T] => Unit): Boolean = {
        writeCallback(pulse); true
      }
    })
  }
}

/** @group create */
object Evt {
  def apply[A]()(implicit ticket: CreationTicket[State]): Evt[A] = {
    ticket.scope.createSource[Pulse[A], Evt[A]](Pulse.NoChange)(init => { new Evt[A](init, ticket.info) }: Evt[A])
  }
}

/** Source signals with imperatively updates.
  *
  * @tparam A Type stored by the signal
  */
class Var[A] private[reactives] (initialState: State[Pulse[A]], name: ReInfo)
    extends Base[Pulse[A]](initialState, name)
    with Source[A] with Signal[A] {
  override type Value = Pulse[A]

  override def disconnect(): Unit = ()

  infix def set(value: A)(using planTransactionScope: PlanTransactionScope[State]): Unit =
    planTransactionScope.planTransaction(this) { admit(value)(_) }

  def transform(f: A => A)(using planTransactionScope: PlanTransactionScope[State]): Unit = {
    planTransactionScope.planTransaction(this) { t =>
      admit(f(t.tx.now(this)))(t)
    }
  }

  def setEmpty()(implicit fac: Scheduler[State]): Unit =
    fac.forceNewTransaction(this)(t => admitPulse(Pulse.empty(info))(t))

  def admitPulse(pulse: Pulse[A])(implicit ticket: AdmissionTicket[State]): Unit = {
    ticket.recordChange(new InitialChange[State] {
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
  def apply[T](initval: T)(implicit ticket: CreationTicket[State]): Var[T] = fromChange(Pulse.Value(initval))
  def empty[T](implicit ticket: CreationTicket[State]): Var[T]             = fromChange(Pulse.empty(ticket.info))
  private def fromChange[T](change: Pulse[T])(implicit ticket: CreationTicket[State]): Var[T] = {
    ticket.scope.createSource[Pulse[T], Var[T]](change)(s => new Var[T](s, ticket.info))
  }
}
