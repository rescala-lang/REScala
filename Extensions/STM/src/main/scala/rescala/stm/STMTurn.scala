package rescala.stm

import rescala.graph.{LevelStruct, Pulse}
import rescala.propagation.LevelBasedPropagation

import scala.concurrent.stm.{InTxn, atomic}

class STMTurn extends LevelBasedPropagation[STMTurn] with LevelStruct {
  override type StructType[P, R] = STMSpore[P, R]

  /** used to create state containers of each reactive */
  override def makeStructState[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): StructType[P, R] = {
    new STMSpore[P, R](initialValue, transient, initialIncoming)
  }
  override def releasePhase(): Unit = ()
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


