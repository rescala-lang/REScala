package rescala.turns

import rescala.graph.{Reactive, Spores}
import rescala.propagation.{AbstractPropagation, PropagationImpl}

abstract class FactoryReference[S <: Spores](override val bufferFactory: S) extends Turn[S]

class EngineImpl[S <: Spores, TImpl <: AbstractPropagation[S]](override private[rescala] val bufferFactory: S, newTurn: => TImpl)  extends EngineImplTrait[S, TImpl] {
  override protected def makeTurn(): TImpl = newTurn
}

trait NoLocking[S <: Spores] extends PropagationImpl[S] {
  override def lockPhase(initialWrites: List[Reactive[S]]): Unit = ()
  override def releasePhase(): Unit = ()
}

