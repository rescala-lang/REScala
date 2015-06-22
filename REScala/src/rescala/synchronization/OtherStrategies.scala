package rescala.synchronization

import rescala.graph.{BufferFactory, Reactive}
import rescala.propagation.PropagationImpl
import rescala.turns.{Engine, Engines, Turn}

import scala.concurrent.stm.{InTxn, atomic}

abstract class BufferFactoryReference[T <: Turn](override val bufferFactory: BufferFactory) extends Turn

trait NoLocking extends PropagationImpl {
  override def lockPhase(initialWrites: List[Reactive]): Unit = ()
  override def releasePhase(): Unit = ()
}

class STMSync extends BufferFactoryReference[STMSync](BufferFactory.stm) with NoLocking {
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


