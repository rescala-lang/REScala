package rescala.synchronization

import rescala.graph.STMSpores
import rescala.propagation.{NoLocking, FactoryReference}

import scala.concurrent.stm.{InTxn, atomic}

class STMSync extends FactoryReference[STMSpores.type](STMSpores) with NoLocking[STMSpores.type] {
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


