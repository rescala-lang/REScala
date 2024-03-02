package reactives.core

import reactives.operator.Interface

case class TransactionScope[State[_]](static: Option[Transaction[State]])
object TransactionScope extends LowPrioTransactionScope {
  inline given search[State[_]](using tx: Transaction[State]): TransactionScope[State]     = static(tx)
  inline given search[State[_]](using at: AdmissionTicket[State]): TransactionScope[State] = static(at.tx)
  inline given Search[State[_]](using st: StaticTicket[State]): TransactionScope[State]    = static(st.tx)

  def static[State[_]](transaction: Transaction[State]): TransactionScope[State] = TransactionScope(Some(transaction))

  // non inline give variant for macro
  def fromTicket[State[_]](ticket: StaticTicket[State]): TransactionScope[State] = TransactionScope(Some(ticket.tx))
}
trait LowPrioTransactionScope {
  given dynamicTransactionScope[State[_]]: TransactionScope[State] = TransactionScope(None)
}

trait CreationScope[State[_]] {
  def embedCreation[T](f: Transaction[State] => T): T

  private[reactives] def create[V, T <: Derived.of[State]](
      incoming: Set[ReSource.of[State]],
      initValue: V,
      needsReevaluation: Boolean
  )(instantiateReactive: State[V] => T): T = {
    embedCreation { tx =>
      val init: Initializer[State] = tx.initializer
      init.create(incoming, initValue, needsReevaluation)(instantiateReactive)
    }
  }

  private[reactives] def createSource[V, T <: ReSource.of[State]](intv: V)(instantiateReactive: State[V] => T): T = {
    embedCreation(_.initializer.createSource(intv)(instantiateReactive))
  }
}

object CreationScope {

  case class StaticCreationScope[State[_]](tx: Transaction[State]) extends CreationScope[State] {
    override def embedCreation[T](f: Transaction[State] => T): T = f(tx)
  }
  case class DynamicCreationScope[State[_]](ds: DynamicScope[State]) extends CreationScope[State] {
    override def embedCreation[T](f: Transaction[State] => T): T = ds.dynamicTransaction(f)
  }

  inline given search(using ts: TransactionScope[Interface.State]): CreationScope[Interface.State] = ts.static match
    case None     => DynamicCreationScope(Interface.default.dynamicScope)
    case Some(tx) => StaticCreationScope(tx)
}

trait PlanTransactionScope[State[_]] {
  def planTransaction(initialWrites: ReSource.of[State]*)(admissionPhase: AdmissionTicket[State] => Unit): Unit
}

object PlanTransactionScope {

  implicit def fromScheduler[State[_]](scheduler: Scheduler[State]): DynamicTransactionLookup[State] =
    DynamicTransactionLookup(scheduler)

  case class StaticInTransaction[State[_]](tx: Transaction[State], scheduler: Scheduler[State])
      extends PlanTransactionScope[State] {
    override def planTransaction(inintialWrites: ReSource.of[State]*)(admission: AdmissionTicket[State] => Unit): Unit =
      tx.observe { () =>
        scheduler.forceNewTransaction(inintialWrites*)(admission)
      }
  }

  case class DynamicTransactionLookup[State[_]](scheduler: Scheduler[State]) extends PlanTransactionScope[State] {
    override def planTransaction(inintialWrites: ReSource.of[State]*)(admission: AdmissionTicket[State] => Unit): Unit =
      scheduler.dynamicScope.maybeTransaction match
        case Some(tx) => tx.observe { () =>
            scheduler.forceNewTransaction(inintialWrites*)(admission)
          }
        case None =>
          scheduler.forceNewTransaction(inintialWrites*)(admission)
  }

  inline given search(using ts: TransactionScope[Interface.State]): PlanTransactionScope[Interface.State] =
    ts.static match
      case None     => DynamicTransactionLookup(Interface.default)
      case Some(tx) => StaticInTransaction(tx, Interface.default)
}
