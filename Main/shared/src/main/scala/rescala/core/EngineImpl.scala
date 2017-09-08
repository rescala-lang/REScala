package rescala.core

import scala.util.DynamicVariable

trait EngineImpl[S <: Struct, ExactTurn <: Turn[S] with Creation[S], LocalTurn <: ExactTurn] extends Engine[S] {

  /**
    * Returns a new turn to be used by the engine
    *
    * @return New turn
    */
  protected def makeTurn(priorTurn: Option[ExactTurn]): LocalTurn

  override private[rescala] def create[T](f: (Creation) => T) = {
    _currentTurn.value match {
      case Some(turn) => f(turn)
      case None => executeTurn(Set.empty, ticket => f(ticket.creation), noWrapUp[T])
    }
  }


  protected val _currentTurn: DynamicVariable[Option[ExactTurn]] = new DynamicVariable[Option[ExactTurn]](None)
  private[rescala] def withTurn[R](turn: ExactTurn)(thunk: => R): R = _currentTurn.withValue(Some(turn))(thunk)
}

