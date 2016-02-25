package rescala.propagation

import rescala.graph.{Reactive, Struct}

trait NoLocking[S <: Struct] extends LevelBasedPropagation[S] {
  override def lockPhase(initialWrites: List[Reactive[S]]): Unit = ()
  override def releasePhase(): Unit = ()
}
