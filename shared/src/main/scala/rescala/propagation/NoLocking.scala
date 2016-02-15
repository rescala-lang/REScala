package rescala.propagation

import rescala.graph.{Reactive, Spores}

trait NoLocking[S <: Spores] extends PropagationImpl[S] {
  override def lockPhase(initialWrites: List[Reactive[S]]): Unit = ()
  override def releasePhase(): Unit = ()
}
