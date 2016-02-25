package rescala.propagation

import rescala.graph.{Reactive, Struct}

trait NoLocking[S <: Struct] extends LevelBasedPropagation[S] {
  override def releasePhase(): Unit = ()
}
