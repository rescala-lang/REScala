package reactives.core
import reactives.core.ReSource.of

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

  inline given search[State[_]]: CreationScope[State] = scala.compiletime.summonFrom {
    case tx: Transaction[State]  => StaticCreationScope(tx)
    case ds: DynamicScope[State] => DynamicCreationScope(ds)
  }
}

trait PlanTransactionScope[State[_]] {
  def planTransaction(initialWrites: ReSource.of[State]*)(admissionPhase: AdmissionTicket[State] => Unit): Unit
}

object PlanTransactionScope {

  case class StaticInTransaction[State[_]](tx: Transaction[State], scheduler: Scheduler[State])
      extends PlanTransactionScope[State] {
    override def planTransaction(inintialWrites: ReSource.of[State]*)(admissionPhase: AdmissionTicket[State] => Unit)
        : Unit =
      tx.observe: () =>
        scheduler.forceNewTransaction(inintialWrites*)(admissionPhase)
  }

  case class DynamicTransactionLookup[State[_]](ds: DynamicScope[State], scheduler: Scheduler[State])
      extends PlanTransactionScope[State] {
    override def planTransaction(inintialWrites: ReSource.of[State]*)(admissionPhase: AdmissionTicket[State] => Unit)
        : Unit =
      ds.maybeTransaction match
        case Some(tx) => tx.observe: () =>
            scheduler.forceNewTransaction(inintialWrites*)(admissionPhase)
        case None =>
          scheduler.forceNewTransaction(inintialWrites*)(admissionPhase)
  }

  inline given summon[State[_]]: PlanTransactionScope[State] = scala.compiletime.summonFrom {
    case scheduler: Scheduler[State] =>
      scala.compiletime.summonFrom {
        case tx: Transaction[State] =>
          StaticInTransaction(tx, scheduler)
        case ds: DynamicScope[State] =>
          DynamicTransactionLookup(ds, scheduler)
      }

  }
}

case class ScopeSearch[State[_]](self: Either[Transaction[State], DynamicScope[State]]) {

  /** Either just use the statically found transaction,
    * or do a lookup in the dynamic scope.
    * If the lookup fails, it will start a new transaction.
    */
  def embedTransaction[T](f: Transaction[State] => T): T =
    self match {
      case Left(integrated) => f(integrated)
      case Right(ds)        => ds.dynamicTransaction(dt => f(dt))
    }

  def maybeTransaction: Option[Transaction[State]] = self match {
    case Left(integrated) => Some(integrated)
    case Right(ds)        => ds.maybeTransaction
  }

}

/** As reactives can be created during propagation, any Ticket can be converted to a creation ticket. */
object ScopeSearch extends LowPriorityScopeImplicits {

  implicit def fromTicketImplicit[S[_]](implicit ticket: StaticTicket[S]): ScopeSearch[S] =
    new ScopeSearch(Left(ticket.tx))
  implicit def fromAdmissionImplicit[S[_]](implicit ticket: AdmissionTicket[S]): ScopeSearch[S] =
    new ScopeSearch(Left(ticket.tx))
  implicit def fromTransactionImplicit[S[_]](implicit tx: Transaction[S]): ScopeSearch[S] =
    new ScopeSearch(Left(tx))
}

/** If no Fitting Ticket is found, then these implicits will search for a [[DynamicScope]],
  * creating the reactives outside of any turn.
  */
sealed trait LowPriorityScopeImplicits {
  implicit def fromSchedulerImplicit[S[_]](implicit factory: DynamicScope[S]): ScopeSearch[S] =
    new ScopeSearch(Right(factory))
}
