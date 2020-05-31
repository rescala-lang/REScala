package rescala.core

import scala.util.DynamicVariable

trait DynamicInitializerLookup[S <: Struct, ExactInitializer <: Initializer[S]] extends Scheduler[S] {

  final override private[rescala] def creationDynamicLookup[T](f: Initializer[S] => T): T = {
    _currentTurn.value match {
      case Some(turn) => f(turn)
      case None => forceNewTransaction(Set.empty, ticket => f(ticket.initializer))
    }
  }

  final protected val _currentTurn: DynamicVariable[Option[ExactInitializer]] =
    new DynamicVariable[Option[ExactInitializer]](None)
  final private[rescala] def withDynamicInitializer[R](turn: ExactInitializer)(thunk: => R): R =
    _currentTurn.withValue(Some(turn))(thunk)
}
