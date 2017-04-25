package rescala.engine

import rescala.graph.Struct

import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.util.DynamicVariable

/**
  * Propagation engine that defines the basic data-types available to the user and creates turns for propagation handling
  *
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  * @tparam TTurn Turn type used by the engine
  */
@implicitNotFound(msg = "Could not find an implicit propagation engine. Did you forget an import?")
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

