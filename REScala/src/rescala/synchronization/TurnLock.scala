package rescala.synchronization

import rescala.graph.Reactive

import scala.annotation.tailrec

final class TurnLock(val guarded: Reactive) {

  override def toString: String = s"Lock($guarded)"

  /** this is guarded by our intrinsic lock */
  private var owner: Key = null

  def getOwner: Key = synchronized(owner)

  /** returns true if key owns the write lock */
  def isOwner(key: Key): Boolean = synchronized(owner eq key)

  /**
   * acquires dynamic acces to the lock.
   * this can block until all other turns waiting on the lock have finished
   */
  def acquireDynamic(key: Key): Unit = request(key)(Keychains.Done(Unit)) {
    owner.keychain.append(key.keychain)
    Keychains.Await
  }

  /**
   * this will block until the lock is owned by the turn.
   * this does not test for shared access and thus will deadlock if the current owner has its locks shared with the turn.
   * use with caution as this can potentially deadlock
   */
  def lock(key: Key): Unit = {
    synchronized { while (tryLock(key) ne key) wait() }
    // wait for master lock to become free
    key.keychain.synchronized(Unit)
  }

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

  @tailrec
  def request(requester: Key)(waiting: => Keychains.Result[Unit])(other: => Keychains.Result[Unit]): Unit = {
    val oldOwner = tryLock(requester)
    val res =
      if (oldOwner eq requester) Keychains.Done(Unit)
      else {
        Keychains.lockKeychains(requester, oldOwner) {
          synchronized {
            tryLock(requester) match {
              // make sure the other owner did not unlock before we got his master lock
              case _ if owner eq requester => Keychains.Done(Unit)
              case _ if owner ne oldOwner => Keychains.Retry
              case _ if requester.keychain eq owner.keychain => waiting
              case _ => other
            }
          }
        }
      }
    res match {
      case Keychains.Await => lock(requester)
      case Keychains.Retry => request(requester)(waiting)(other)
      case Keychains.Done(_) =>
    }
  }

  /**
   * transfers the lock from the turn to the target.
   * this notifies all turns waiting on this lock because we need the turn the lock was transferred to to wake up
   */
  def transfer(target: Key, oldOwner: Key) = synchronized {
    if (!isOwner(oldOwner)) throw new IllegalMonitorStateException(s"$this is held by $owner but tried to transfer by $oldOwner (to $target)")
    owner = target
    if (target ne null) target.addLock(this)
    notifyAll()
  }

}

