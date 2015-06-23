package rescala.synchronization

import rescala.graph.{JVMFactories, SynchronizationFactory, Reactive}
import rescala.propagation.PropagationImpl
import rescala.turns.{Engine, Turn}

import scala.concurrent.stm.{InTxn, atomic}

class STMSync extends FactoryReference[STMSync](JVMFactories.stm) with NoLocking {
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


