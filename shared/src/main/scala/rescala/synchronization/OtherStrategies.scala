package rescala.synchronization

import rescala.graph.{Reactive, SynchronizationFactory}
import rescala.propagation.PropagationImpl
import rescala.turns.Turn

abstract class FactoryReference[T <: Turn](override val bufferFactory: SynchronizationFactory) extends Turn

trait NoLocking extends PropagationImpl {
  override def lockPhase(initialWrites: List[Reactive]): Unit = ()
  override def releasePhase(): Unit = ()
}

