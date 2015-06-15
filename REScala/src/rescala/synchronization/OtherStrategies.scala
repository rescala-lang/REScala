package rescala.synchronization

import rescala.graph.{BufferFactory, Reactive}
import rescala.propagation.PropagationImpl
import rescala.turns.{Engine, Engines, Turn}

import scala.concurrent.stm.{InTxn, atomic}

abstract class EngineReference[T <: Turn](override val bufferFactory: BufferFactory) extends Turn

trait NoLocking extends PropagationImpl {
  override def lockPhase(initialWrites: List[Reactive]): Unit = ()
  override def realeasePhase(): Unit = ()
}

class STMSync extends EngineReference[STMSync](BufferFactory.stm) with NoLocking {
  // this is unsafe when used improperly
  def inTxn: InTxn = atomic(identity)
}


