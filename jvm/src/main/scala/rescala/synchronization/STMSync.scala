package rescala.synchronization

import rescala.graph.STMState

import scala.concurrent.stm.{InTxn, atomic}

class STMSync extends FactoryReference[STMState.type ](STMState) with NoLocking[STMState.type] {
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


