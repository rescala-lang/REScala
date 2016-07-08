package rescala.engines

import rescala.graph.Struct
import rescala.propagation.AbstractPropagation

/**
  * Implementation of the turn creation function based on a given function as class parameter
  *
  * @param newTurn Function that returns a new turn to be used by the engine
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  * @tparam TImpl Turn type used by the engine
  */
class EngineImpl[S <: Struct, TImpl <: AbstractPropagation[S]](newTurn: => TImpl)  extends PlanImpl[S, TImpl] {
  override protected def makeTurn(): TImpl = newTurn
}
