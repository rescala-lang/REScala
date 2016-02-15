package rescala.stm

import rescala.propagation.{FactoryReference, NoLocking}

import scala.concurrent.stm.{InTxn, atomic}

class STMEngine extends FactoryReference[STMSpores.type](STMSpores) with NoLocking[STMSpores.type] {
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


