package rescala.core

import scala.util.DynamicVariable

trait DynamicInitializerLookup[S <: Struct, ExactInitializer <: Initializer[S]] extends Scheduler[S] {

  final override private[rescala] def initializerDynamicLookup[T](f: Initializer[S] => T): T = {
    _currentInitializer.value match {
      case Some(turn) => f(turn)
      case None       => forceNewTransaction(Set.empty, ticket => f(ticket.initializer))
    }
  }

  final protected val _currentInitializer: DynamicVariable[Option[ExactInitializer]] =
    new DynamicVariable[Option[ExactInitializer]](None)
  final private[rescala] def withDynamicInitializer[R](init: ExactInitializer)(thunk: => R): R =
    _currentInitializer.withValue(Some(init))(thunk)
}
