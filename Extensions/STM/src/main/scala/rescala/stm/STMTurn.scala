package rescala.stm

import rescala.graph.Pulse
import rescala.levelbased.{LevelBasedPropagation, LevelStruct}

import scala.concurrent.stm.{InTxn, atomic}

class STMTurn extends LevelBasedPropagation[STMTurn] with LevelStruct {
  override type Type[P, R] = STMStructType[P, R]

  /** used to create state containers of each reactive */
  override def makeStructState[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): Type[P, R] = {
    new STMStructType[P, R](initialValue, transient, initialIncoming)
  }
  override def releasePhase(): Unit = ()
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


