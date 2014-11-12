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
}
