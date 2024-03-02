package reactives.core

import reactives.operator.Interface

case class TransactionSearch[State[_]](static: Option[Transaction[State]])
object TransactionSearch extends LowPrioTransactionScope {
  inline given search[State[_]](using tx: Transaction[State]): TransactionSearch[State]     = static(tx)
  inline given search[State[_]](using at: AdmissionTicket[State]): TransactionSearch[State] = static(at.tx)
  inline given Search[State[_]](using st: StaticTicket[State]): TransactionSearch[State]    = static(st.tx)

  def static[State[_]](transaction: Transaction[State]): TransactionSearch[State] = TransactionSearch(Some(transaction))

  // non inline give variant for macro
  def fromTicket[State[_]](ticket: StaticTicket[State]): TransactionSearch[State] = TransactionSearch(Some(ticket.tx))
}
trait LowPrioTransactionScope {
  given dynamicTransactionScope[State[_]]: TransactionSearch[State] = TransactionSearch(None)
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

  inline given search(using ts: TransactionSearch[Interface.State]): CreationScope[Interface.State] = ts.static match
    case None     => DynamicCreationScope(reactives.default.global.dynamicScope)
    case Some(tx) => StaticCreationScope(tx)
}

trait PlanTransactionScope[State[_]] {
  def planTransaction(initialWrites: ReSource.of[State]*)(admissionPhase: AdmissionTicket[State] => Unit): Unit
}

object PlanTransactionScope {

  case class StaticInTransaction[State[_]](tx: Transaction[State], scheduler: Scheduler[State])
      extends PlanTransactionScope[State] {
    override def planTransaction(inintialWrites: ReSource.of[State]*)(admission: AdmissionTicket[State] => Unit): Unit =
      tx.observe { () =>
        scheduler.forceNewTransaction(inintialWrites*)(admission)
      }
  }

  case class DynamicTransactionLookup[State[_]](scheduler: Scheduler[State], dynamicScope: DynamicScope[State])
      extends PlanTransactionScope[State] {
    override def planTransaction(inintialWrites: ReSource.of[State]*)(admission: AdmissionTicket[State] => Unit): Unit =
      dynamicScope.maybeTransaction match
        case Some(tx) => tx.observe { () =>
            scheduler.forceNewTransaction(inintialWrites*)(admission)
          }
        case None =>
          scheduler.forceNewTransaction(inintialWrites*)(admission)
  }

  inline given search(using ts: TransactionSearch[Interface.State]): PlanTransactionScope[Interface.State] =
    ts.static match
      case None => DynamicTransactionLookup(reactives.default.global.scheduler, reactives.default.global.dynamicScope)
      case Some(tx) => StaticInTransaction(tx, reactives.default.global.scheduler)
}
