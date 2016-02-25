package rescala.propagation

import rescala.graph.{LevelStruct, Reactive, Struct}

trait NoLocking[S <: LevelStruct] extends LevelBasedPropagation[S] {
  override def releasePhase(): Unit = ()
}
