package rescala.synchronization

import rescala.graph.Reactive
import rescala.propagation.TurnImpl
import rescala.turns.{Engine, Engines, Turn}

import scala.annotation.tailrec
import scala.concurrent.stm.{InTxn, atomic}

abstract class EngineReference[T <: Turn](override val engine: Engine[T]) extends Turn

class Pessimistic extends EngineReference[Pessimistic](Engines.pessimistic) with Prelock {
  /**
   * this is called after the initial closure of the turn has been executed,
   * that is the eval queue is populated with the sources
   */
  override def lockPhase(initialWrites: List[Reactive]): Unit = SyncUtil.lockReachable(initialWrites, r => { acquireWrite(r); true })

  /**
   * acquires write acces to the lock.
   * this can cause a temporary loss off all locks held by key,
   * so key needs to be in a clean state before this is called.
   * this can block until other turns waiting on the lock have finished
   */
  def acquireWrite(reactive: Reactive): Unit = {
    import reactive.lock._
    acquireDynamic(key)
    if (!hasWriteAccess(key)) {
      key.withMaster {
        val subs = key.subsequent.get
        subs.withMaster {
          // release locks so that whatever waits for us can continue
          key.releaseAll()
          // but in turn we wait on that
          key.appendAfter(subs)
        }
      }
      // can now safely wait as we will get the lock eventually
      lock(key)
    }
  }
}

class Yielding extends EngineReference[Yielding](Engines.yielding) with Prelock {
  /**
   * this is called after the initial closure of the turn has been executed,
   * that is the eval queue is populated with the sources
   */
  override def lockPhase(initialWrites: List[Reactive]): Unit =
    SyncUtil.lockReachable(initialWrites, r => { acquireWrite(r); true })

  /**
   * acquires write acces to the lock.
   * this can cause a temporary loss off all locks held by key,
   * so key needs to be in a clean state before this is called.
   * this can block until other turns waiting on the lock have finished
   */
  @tailrec
  private def acquireWrite(reactive: Reactive): Unit = {
    import reactive.lock._
    val oldOwner = tryLock(key)
    val res: Symbol = if (oldOwner eq key) 'done
    else {
      SyncUtil.lockLanes(key, oldOwner) {
        reactive.lock.synchronized {
          tryLock(key) match {
            // make sure the other owner did not unlock before we got his master lock
            case newOwner if newOwner eq key => 'done
            case newOwner if newOwner ne oldOwner => 'retry
            case newOwner if SyncUtil.controls(key, newOwner) => key.subsequent.get.withMaster {
              // cycle
              key.releaseAll()
              key.appendAfter(newOwner)
              'await
            }
            case newOwner =>
              // yield
              key.transferAll(SyncUtil.laneHead(newOwner))
              key.appendAfter(newOwner)
              'await
          }
        }
      }
    }
    res match {
      case 'await => lock(key)
        key.withMaster(Unit)
      case 'retry => acquireWrite(reactive)
      case 'done =>
    }
  }
}

trait NothingSpecial extends TurnImpl {
  override def lockPhase(initialWrites: List[Reactive]): Unit = ()
  override def realeasePhase(): Unit = ()
}

class STMSync extends EngineReference[STMSync](Engines.STM) with NothingSpecial {
  // this is a horrible idea
  def inTxn: InTxn = atomic(identity)
}

class SpinningInitPessimistic extends EngineReference[SpinningInitPessimistic](Engines.spinningInit) with Prelock {

  override def lockPhase(initialWrites: List[Reactive]): Unit = SyncUtil.lockReachable(initialWrites, acquireWrite)

  def acquireWrite(reactive: Reactive): Boolean =
    if (reactive.lock.tryLock(key) eq key) true
    else {
      key.withMaster { key.releaseAll() }
      false
    }

}
