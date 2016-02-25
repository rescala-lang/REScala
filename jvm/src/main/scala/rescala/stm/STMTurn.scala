package rescala.stm

import rescala.propagation.{NoLocking}

import scala.concurrent.stm.{InTxn, atomic}

class STMTurn extends NoLocking[STMStruct.type] {

  /** used to create state containers of each reactive */
  override def bufferFactory: STMStruct.type = STMStruct
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


