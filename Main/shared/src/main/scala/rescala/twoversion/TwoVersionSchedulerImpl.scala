package rescala.twoversion

import rescala.core.Initializer

/**
  * Implementation of the turn creation function based on a given function as class parameter
  *
  * @param newTurn Function that returns a new turn to be used by the engine
  * @tparam S     Struct type that defines the spore type used to manage the reactive evaluation
  * @tparam TImpl Turn type used by the engine
  */
class TwoVersionSchedulerImpl[S <: TwoVersionStruct, TImpl <: TwoVersionPropagation[S] with Initializer[S]]
(override val schedulerName: String, newTurn: Option[TImpl] => TImpl)
  extends TwoVersionScheduler[S, TImpl] {

  override protected def makeTurn(priorTurn: Option[TImpl]): TImpl = newTurn(priorTurn)
}
