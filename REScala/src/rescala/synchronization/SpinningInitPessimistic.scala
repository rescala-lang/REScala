package rescala.synchronization

import rescala.graph.Reactive
import rescala.turns.{Engine, Engines, Turn}

class SpinningInitPessimistic extends Pessimistic {
  /** returns the engine of this turn */
  override val engine: Engine[Turn] = Engines.spinningInit

  override def lockPhase(initialWrites: List[Reactive]): Unit = SyncUtil.lockReachable(initialWrites, acquireWrite )
  
  def acquireWrite(reactive: Reactive): Boolean =
    if (reactive.lock.tryLock(key) eq key) true
    else {
      key.withMaster { key.releaseAll() }
      false
    }

}
