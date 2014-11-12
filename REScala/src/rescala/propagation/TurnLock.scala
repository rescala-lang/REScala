package rescala.propagation

import java.util.concurrent.locks.ReentrantLock

class TurnLock {

  val relock: ReentrantLock = new ReentrantLock()
  var owner: Option[Turn] = None
  def lock()(implicit turn: Turn): Unit = {
    relock.lock()
    owner = Some(turn)
  }
  def tryLock()(implicit turn: Turn): Boolean = {
    val res = relock.tryLock()
    if (res) owner = Some(turn)
    res
  }
  def unlock(): Unit = {
    if(relock.getHoldCount == 1) owner = None
    relock.unlock()
  }

  def steal()(implicit turn: Turn): Unit = {
    if (turn.stealRights.contains(owner.get)) {
      //TODO: do something to move the lock or maybe make lock and try lock fall back to stealing
    }
  }

  def tradeStealRights()(implicit turn: Turn): Unit = {
    val Some(own) = owner
    if (!turn.stealRights.contains(own)) {
      if (System.identityHashCode(own) < System.identityHashCode(turn)) {
        own.stealLock.lock()
        turn.stealLock.lock()
      }
      else {
        turn.stealLock.lock()
        own.stealLock.lock()
      }

      if (!turn.stealRights.contains(own)) {
        own.stealRights += turn
        own.stealRightsAcquired.notify()
      }

      own.stealLock.unlock()
      while (!turn.stealRights.contains(own))
        turn.stealRightsAcquired.await()
      turn.stealLock.unlock()
    }
  }
}
