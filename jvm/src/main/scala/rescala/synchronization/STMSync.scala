package rescala.synchronization

import rescala.graph.JVMFactories.STMState
import rescala.graph.{JVMFactories, State, Reactive}
import rescala.propagation.PropagationImpl
import rescala.turns.{Engine, Turn}

import scala.concurrent.stm.{InTxn, atomic}

class STMSync extends FactoryReference[STMState](JVMFactories.stm) with NoLocking[STMState] {
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


