package rescala.synchronization

import rescala.graph.Reactive
import rescala.turns.Turn

import scala.collection.immutable.Queue

final class TurnLock(val guarded: Reactive) {
  override def toString: String = s"Lock($guarded)"

  /** this is guarded by our intrinsic lock */
  private var owner: Turn = null
  private var shared: Queue[Turn] = Queue()

  def getOwner: Turn = synchronized(owner)

  /** returns true if key owns the write lock */

  def isOwner(key: Turn): Boolean = synchronized(owner eq key)


  /**
   * locks this if it is free, returns the current owner (which is key, if locking succeeded)
   * does not check for shared access.
   */
  def tryLock(key: Turn): Turn = synchronized {
    if (owner eq null) {
      owner = key
      key.addLock(this)
    }
    owner
  }

  def share(key: Turn) = synchronized(shared = shared.enqueue(key))
  def acquired(key: Turn) = synchronized {
    val (k, r) = shared.dequeue
    assert(k == key, s"resolved await in wrong order got $k expected $key remaining $r")
    shared = r
    key
  }

  /** transfers the lock from the turn to the target. */
  def transfer(target: Turn, oldOwner: Turn, ignoreShared: Boolean = false) = synchronized {
    assert(owner eq oldOwner, s"$this is held by $owner but tried to transfer by $oldOwner (to $target)")
    if (!ignoreShared && shared.isEmpty) owner = null
    else {
      owner = target
      if (target ne null) target.addLock(this)
    }
  }

}

