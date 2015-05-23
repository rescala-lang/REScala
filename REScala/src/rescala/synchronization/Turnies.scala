package rescala.synchronization

import rescala.graph.Reactive
import rescala.propagation.TurnImpl
import rescala.turns.{Engine, Engines, Turn}
import scala.concurrent.stm.{InTxn, atomic}
import rescala.propagation.PropagateChangesOnly
import rescala.propagation.PropagateChangesOnly

abstract class EngineReference[T <: Turn](override val engine: Engine[T]) extends Turn

trait NothingSpecial extends TurnImpl with PropagateChangesOnly{
  override def lockPhase(initialWrites: List[Reactive]): Unit = ()
  override def releasePhase(): Unit = ()
}

class STMSync extends EngineReference[STMSync](Engines.STM) with NothingSpecial {
  // this is a horrible idea
  def inTxn: InTxn = atomic(identity)
}

class SpinningInitPessimistic(var backOff: Int) extends EngineReference[SpinningInitPessimistic](Engines.spinning) with Prelock with PropagateChangesOnly {

  var currentBackOff = backOff

  override def lockPhase(initialWrites: List[Reactive]): Unit = Keychains.lockReachable(initialWrites, acquireWrite)

  def acquireWrite(reactive: Reactive): Boolean =
    if (reactive.lock.tryLock(this) eq this) true
    else {
      lockKeychain {
        releaseAll()
        keychain = new Keychain(this)
      }
      if (currentBackOff == 0) {
        AcquireShared(reactive, this)
        backOff /= 2
        currentBackOff = backOff
      }
      else if (currentBackOff > 0) {
        currentBackOff -= 1
      }
      false
    }
}
