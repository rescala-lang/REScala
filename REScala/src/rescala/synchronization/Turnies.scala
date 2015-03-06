package rescala.synchronization

import rescala.graph.Reactive
import rescala.propagation.TurnImpl
import rescala.turns.{Engine, Engines, Turn}

import scala.concurrent.stm.{InTxn, atomic}

abstract class EngineReference[T <: Turn](override val engine: Engine[T]) extends Turn

trait NothingSpecial extends TurnImpl {
  override def lockPhase(initialWrites: List[Reactive]): Unit = ()
  override def realeasePhase(): Unit = ()
}

class STMSync extends EngineReference[STMSync](Engines.STM) with NothingSpecial {
  // this is a horrible idea
  def inTxn: InTxn = atomic(identity)
}

class SpinningInitPessimistic(var backOff: Int) extends EngineReference[SpinningInitPessimistic](Engines.spinning) with Prelock {

  var currentBackOff = backOff

  override def lockPhase(initialWrites: List[Reactive]): Unit = Keychains.lockReachable(initialWrites, acquireWrite)

  def acquireWrite(reactive: Reactive): Boolean =
    if (reactive.lock.tryLock(key) eq key) true
    else {
      key.lockKeychain {
        key.releaseAll()
        key.keychain = new Keychain(key)
      }
      if (currentBackOff == 0) {
        AcquireShared(reactive, key)
        backOff /= 2
        currentBackOff = backOff
      }
      else if (currentBackOff > 0) {
        currentBackOff -= 1
      }
      false
    }

}
