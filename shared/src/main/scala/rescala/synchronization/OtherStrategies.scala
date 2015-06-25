package rescala.synchronization

import rescala.graph.{Reactive, Spores}
import rescala.propagation.PropagationImpl
import rescala.turns.Turn

abstract class FactoryReference[S <: Spores](override val bufferFactory: S) extends Turn[S]

trait NoLocking[S <: Spores] extends PropagationImpl[S] {
  override def lockPhase(initialWrites: List[Reactive[S]]): Unit = ()
  override def releasePhase(): Unit = ()
}

