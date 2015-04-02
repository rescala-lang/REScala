package rescala.synchronization

import rescala.graph.Reactive

import scala.collection.immutable.Queue

final class TurnLock(val guarded: Reactive) {
  override def toString: String = s"Lock($guarded)"

  /** this is guarded by our intrinsic lock */
  private var owner: Key = null
  private var shared: Queue[Key] = Queue()
  private var written = false

  def getOwner: Key = synchronized(owner)

  /** returns true if key owns the write lock */
  def isOwner(key: Key): Boolean = synchronized(owner eq key)
  
  def markWritten() = written = true
  def isWritten() = written

  /**
   * locks this if it is free, returns the current owner (which is key, if locking succeeded)
   * does not check for shared access.
   */
  def tryLock(key: Key): Key = synchronized {
    if (owner eq null) {
      owner = key
      key.addLock(this)
    }
    owner
  }

  def share(key: Key) = synchronized(shared = shared.enqueue(key))
  def acquired(key: Key) = synchronized {
    val (k, r) = shared.dequeue
    assert(k == key, s"resolved await in wrong order got $k expected $key remaining $r")
    shared = r
    key
  }

  /** transfers the lock from the turn to the target. */
  def transfer(target: Key, oldOwner: Key, ignoreShared: Boolean = false) = synchronized {
    assert(owner eq oldOwner, s"$this is held by $owner but tried to transfer by $oldOwner (to $target)")
    if (!ignoreShared && shared.isEmpty) owner = null
    else {
      owner = target
      if (target ne null) target.addLock(this)
    }
  }

}

