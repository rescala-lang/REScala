package rescala.synchronization

import rescala.propagation.TurnImpl
import rescala.turns.Engines

import scala.concurrent.stm.{InTxn, atomic}

class STMSync extends TurnImpl(Engines.STM) {
  // this is a horrible idea
  def inTxn: InTxn = atomic(identity)
}
