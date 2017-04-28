package rescala.engine

import rescala.graph.Struct

import scala.language.experimental.macros
import scala.util.DynamicVariable

trait EngineImpl[S <: Struct, TTurn <: Turn[S]] extends Engine[S, TTurn] {
  override private[rescala] def executeTurn[R](initialWrites: Traversable[Reactive], admissionPhase: TTurn => R): R = {
    val turn = makeTurn(initialWrites, currentTurn())
    withTurn(Some(turn))(executeTurn(turn, initialWrites, admissionPhase))
  }

  /**
    * Returns a new turn to be used by the engine
    *
    * @return New turn
    */
  protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[TTurn]): TTurn
  protected def executeTurn[R](turn: TTurn, initialWrites: Traversable[Reactive], admissionPhase: TTurn => R): R

  private val _currentTurn: DynamicVariable[Option[TTurn]] = new DynamicVariable[Option[TTurn]](None)
  override private[rescala] def currentTurn(): Option[TTurn] = _currentTurn.value
  private[rescala] def withTurn[R](turn: Option[TTurn])(thunk: => R): R = _currentTurn.withValue(turn)(thunk)
}

