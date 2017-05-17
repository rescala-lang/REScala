package rescala.engine

import rescala.graph.Struct

import scala.util.DynamicVariable

trait EngineImpl[S <: Struct, TTurn <: Turn[S]] extends Engine[S, TTurn] {
  override protected[rescala] def executeTurn[R](initialWrites: Traversable[Reactive], admissionPhase: TTurn => R): R = {
    val turn = makeTurn(initialWrites, currentTurn())
    executeInternal(turn, initialWrites, () => withTurn(turn){ admissionPhase(turn) })
  }

  /**
    * Returns a new turn to be used by the engine
    *
    * @return New turn
    */
  protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[TTurn]): TTurn
  protected def executeInternal[R](turn: TTurn, initialWrites: Traversable[Reactive], admissionPhase: () => R): R

  private val _currentTurn: DynamicVariable[Option[TTurn]] = new DynamicVariable[Option[TTurn]](None)
  override private[rescala] def currentTurn(): Option[TTurn] = _currentTurn.value
  private[rescala] def withTurn[R](turn: TTurn)(thunk: => R): R = _currentTurn.withValue(Some(turn))(thunk)
}

