package reactives.operator

import reactives.SelectedScheduler.State
import reactives.core.{AdmissionTicket, Base, CreationTicket, InitialChange, Observation, PlanTransactionScope, ReInfo, ReSource, Scheduler}
import reactives.structure.Pulse

trait Source[T] extends reactives.core.ReSource {
  final def admit(value: T)(using ticket: AdmissionTicket[State]): Unit = admitPulse(Pulse.Value(value))
  def admitPulse(pulse: Pulse[T])(using ticket: AdmissionTicket[State]): Unit
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
  infix def fire(value: T)(using planTransactionScope: PlanTransactionScope[State]): Unit =
    planTransactionScope.planTransaction(this)(admit(value)(using _))
  override def disconnect(): Unit = ()
  def admitPulse(pulse: Pulse[T])(using ticket: AdmissionTicket[State]): Unit = {
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
  def apply[A]()(using ticket: CreationTicket[State]): Evt[A] = {
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
    planTransactionScope.planTransaction(this) { admit(value)(using _) }

  def transform(f: A => A)(using planTransactionScope: PlanTransactionScope[State]): Unit = {
    planTransactionScope.planTransaction(this) { t =>
      admit(f(t.tx.now(this)))(using t)
    }
  }

  def setEmpty()(using fac: Scheduler[State]): Unit =
    fac.forceNewTransaction(this)(t => admitPulse(Pulse.empty(info))(using t))

  def admitPulse(pulse: Pulse[A])(using ticket: AdmissionTicket[State]): Unit = {
    ticket.recordChange(new InitialChange[State] {
      override val source: Var.this.type = Var.this
      override def writeValue(base: Pulse[A], writeCallback: Pulse[A] => Unit): Boolean =
        if base != pulse then { writeCallback(pulse); true }
        else false
    })
  }
}

/** Creates new [[Var]]s
  * @group create
  */
object Var {
  def apply[T](initval: T)(using ticket: CreationTicket[State]): Var[T] = fromChange(Pulse.Value(initval))
  def empty[T](using ticket: CreationTicket[State]): Var[T]             = fromChange(Pulse.empty(ticket.info))
  private def fromChange[T](change: Pulse[T])(using ticket: CreationTicket[State]): Var[T] = {
    ticket.scope.createSource[Pulse[T], Var[T]](change)(s => new Var[T](s, ticket.info))
  }
}
