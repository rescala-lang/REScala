package rescala.twoversion

import rescala.core.Initializer

/**
  * Implementation of the turn creation function based on a given function as class parameter
  *
  * @param newTurn Function that returns a new turn to be used by the engine
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  * @tparam TImpl Turn type used by the engine
  */
class TwoVersionSchedulerImpl[S <: TwoVersionStruct, TImpl <: TwoVersionPropagation[S] with Initializer[S]]
(name: String, newTurn: (TwoVersionSchedulerImpl[S,TImpl], Option[TImpl]) => TImpl) extends TwoVersionScheduler[S, TImpl] {
  def this(name: String, newTurn: () => TImpl) = this(name, (_, _) => newTurn())

  override protected def makeTurn(priorTurn: Option[TImpl]): TImpl = newTurn(this, priorTurn)
  lazy override val toString: String = s"Engine($name)"
}
