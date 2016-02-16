package rescala.engines

import rescala.graph.Struct
import rescala.propagation.AbstractPropagation

class EngineImpl[S <: Struct, TImpl <: AbstractPropagation[S]](override private[rescala] val bufferFactory: S, newTurn: => TImpl)  extends PlanImpl[S, TImpl] {
  override protected def makeTurn(): TImpl = newTurn
}
