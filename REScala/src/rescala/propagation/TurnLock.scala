package rescala.propagation

import java.util.concurrent.locks.{AbstractQueuedSynchronizer, ReentrantLock}

import rescala.propagation.turns.Pessimistic

class TurnLock {

  private val sync = new Sync()
  @volatile var owner: Pessimistic = null

  def id(implicit turn: Pessimistic): Int = System.identityHashCode(turn)

  def lock()(implicit turn: Pessimistic): Unit = synchronized {
    sync.acquire(id)
    owner = turn
  }

  def tryLock()(implicit turn: Pessimistic): Boolean = synchronized {
    val res = sync.tryAcquire(id)
    if (res) owner = turn
    res
  }

  def unlock()(implicit turn: Pessimistic): Unit = synchronized {
    if (sync.isOwner(id)) owner = null
    sync.release(id)
  }

  def shared(implicit turn: Pessimistic): Boolean = (turn.shareFrom ne null) && sync.isOwner(id(turn.shareFrom))

  def tradeLocks()(implicit turn: Pessimistic): Boolean = synchronized {
    val ownr = owner
    if (ownr eq null) false
    else {
      if (id(ownr) < id(turn)) {
        ownr.tradeLock.lock()
        turn.tradeLock.lock()
      }
      else {
        turn.tradeLock.lock()
        ownr.tradeLock.lock()
      }

      ownr.shareFrom = turn
      ownr.tradeCondition.notify()

      ownr.tradeLock.unlock()
      while (turn.shareFrom ne owner)
        turn.tradeCondition.await()
      turn.tradeLock.unlock()

      true
    }
  }
}

private class Sync extends AbstractQueuedSynchronizer {
  override def tryAcquire(id: Int): Boolean = isOwner(id) || compareAndSetState(0, id)
  override def tryRelease(id: Int): Boolean = compareAndSetState(id, 0)
  def isOwner(id: Int): Boolean = getState == id
}
