package rescala.synchronization

import rescala.graph.{JVMFactory, SynchronizationFactory, Reactive}
import rescala.propagation.PropagationImpl
import rescala.turns.{Engine, Turn}

import scala.concurrent.stm.{InTxn, atomic}

class STMSync extends FactoryReference[STMSync](JVMFactory.stm) with NoLocking {
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


