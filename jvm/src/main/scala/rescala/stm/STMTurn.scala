package rescala.stm

import rescala.propagation.{FactoryReference, NoLocking}

import scala.concurrent.stm.{InTxn, atomic}

class STMTurn extends FactoryReference[STMStruct.type](STMStruct) with NoLocking[STMStruct.type] {
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


