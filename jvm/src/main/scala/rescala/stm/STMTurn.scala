package rescala.stm

import rescala.propagation.LevelBasedPropagation

import scala.concurrent.stm.{InTxn, atomic}

class STMTurn extends LevelBasedPropagation[STMStruct.type] {

  /** used to create state containers of each reactive */
  override def bufferFactory: STMStruct.type = STMStruct
  override def releasePhase(): Unit = ()
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


