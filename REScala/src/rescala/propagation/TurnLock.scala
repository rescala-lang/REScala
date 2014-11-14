package rescala.propagation

import java.util.concurrent.locks.{AbstractQueuedSynchronizer, ReentrantLock}

class TurnLock {

  private val sync = new Sync()
  @volatile var owner: Turn = null

  def id(implicit turn: Turn): Int = System.identityHashCode(turn)

  def lock()(implicit turn: Turn): Unit = synchronized {
    sync.acquire(id)
    owner = turn
  }

  def tryLock()(implicit turn: Turn): Boolean = synchronized {
    val res = sync.tryAcquire(id)
    if (res) owner = turn
    res
  }

  def unlock()(implicit turn: Turn): Unit = synchronized {
    if (sync.isOwner(id)) owner = null
    sync.release(id)
  }

  def shared(implicit turn: Turn): Boolean = (turn.shareFrom ne null) && sync.isOwner(id(turn.shareFrom))

  def tradeLocks()(implicit turn: Turn): Boolean = synchronized {
    val own = owner
    if (own eq null) false
    else {
      if (id(own) < id(turn)) {
        own.tradeLock.lock()
        turn.tradeLock.lock()
      }
      else {
        turn.tradeLock.lock()
        own.tradeLock.lock()
      }

      own.shareFrom = turn
      own.tradeCondition.notify()

      own.tradeLock.unlock()
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
