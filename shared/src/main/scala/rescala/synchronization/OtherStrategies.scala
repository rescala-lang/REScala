package rescala.synchronization

import rescala.graph.{Reactive, State}
import rescala.propagation.PropagationImpl
import rescala.turns.Turn

abstract class FactoryReference[S <: State](override val bufferFactory: S) extends Turn[S]

trait NoLocking[S <: State] extends PropagationImpl[S] {
  override def lockPhase(initialWrites: List[Reactive[S]]): Unit = ()
  override def releasePhase(): Unit = ()
}

