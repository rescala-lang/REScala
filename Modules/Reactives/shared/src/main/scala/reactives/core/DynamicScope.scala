package reactives.core

import scala.util.DynamicVariable

trait SchedulerWithDynamicScope[State[_], Tx <: Transaction[State]] extends Scheduler[State] {
  def dynamicScope: DynamicScopeImpl[State, Tx] = new DynamicScopeImpl[State, Tx](this)
}

/** Provides the capability to look up transactions in the dynamic scope. */
trait DynamicScope[State[_]] {
  private[reactives] def dynamicTransaction[T](f: Transaction[State] ?=> T): T
  def maybeTransaction: Option[Transaction[State]]
}

class DynamicScopeImpl[State[_], Tx <: Transaction[State]](scheduler: SchedulerWithDynamicScope[State, Tx])
    extends DynamicScope[State] {

  final private[reactives] def dynamicTransaction[T](f: Transaction[State] ?=> T): T = {
    _currentTransaction.value match {
      case Some(transaction) => f(using transaction)
      case None              => scheduler.forceNewTransaction(Set.empty, ticket => f(using ticket.tx))
    }
  }

  final protected val _currentTransaction: DynamicVariable[Option[Tx]] =
    new DynamicVariable[Option[Tx]](None)

  final private[reactives] def withDynamicInitializer[R](init: Tx)(thunk: => R): R =
    _currentTransaction.withValue(Some(init))(thunk)

  final override def maybeTransaction: Option[Tx] = {
    _currentTransaction.value
  }

}
