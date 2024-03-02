package reactives.core
import reactives.core.ReSource.of
import reactives.operator.Signal

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

  def makeDynamicIndirectionForMacroReplacement[State[_]](ds: DynamicScope[State]): DynamicCreationScope[State] =
    new DynamicCreationScope(ds)
  def makeFromTicket[State[_]](tick: StaticTicket[State]): StaticCreationScope[State] = new StaticCreationScope(tick.tx)

  inline given search[State[_]]: CreationScope[State] = scala.compiletime.summonFrom {
    case tx: Transaction[State]  => StaticCreationScope(tx)
    case ds: DynamicScope[State] => makeDynamicIndirectionForMacroReplacement(ds)
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
      tx.observe { () =>
        scheduler.forceNewTransaction(inintialWrites*)(admissionPhase)
      }
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
