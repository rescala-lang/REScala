package rescala.synchronization

import rescala.graph.Reactive

import scala.annotation.tailrec

final class TurnLock(val guarded: Reactive) {

  override def toString: String = s"Lock($guarded)"

  /** this is guarded by our intrinsic lock */
  private var owner: Key = null

  def getOwner: Key = synchronized(owner)

  /** returns true if key owns the write lock */
  def hasWriteAccess(key: Key): Boolean = synchronized(owner eq key)

  /**
   * if key has dynamic access, he is guaranteed that no one else will write guarded reactive
   * and key is allowed to write the sinks of the reactive, but key must ensure that the owner is informed
   * about added reactives by calling the owners handleDependencyChange
   */
  def hasDynamicAccess(key: Key): Boolean = synchronized {
    if (owner == null) throw new IllegalStateException
    SyncUtil.controls(key, getOwner)
  }

  /**
   * acquires dynamic acces to the lock.
   * this can block until all other turns waiting on the lock have finished
   */
  def acquireDynamic(key: Key): Unit = {
    if (synchronized {
      tryLock(key) != key && !hasDynamicAccess(key)
    }) request(key)
  }

  /**
   * this will block until the lock is owned by the turn.
   * this does not test for shared access and thus will deadlock if the current owner has its locks shared with the turn.
   * use with caution as this can potentially deadlock
   */
  def lock(key: Key): Unit = synchronized {
    while (tryLock(key) ne key) wait()
    key.synchronized(Unit)
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

  /**
   * request basically means that the turn will share all its locks with the owner of the current lock
   * and in exchange request that the owner will transfer all of its locks to the turn when he is finished.
   */
  @tailrec
  private def request(requester: Key): Unit = {
    val oldOwner = tryLock(requester)
    val res = if (oldOwner eq requester) 'done
    else {
      SyncUtil.lockLanes(requester, oldOwner) {
        synchronized {
          tryLock(requester) match {
            // make sure the other owner did not unlock before we got his master lock
            case newOwner if newOwner eq requester => 'done
            case newOwner if newOwner ne oldOwner => 'retry
            // test makes sure, that owner is not waiting on us
            case _ if hasDynamicAccess(requester) => 'done
            // trade our rights
            case _ =>
              requester.appendAfter(owner)
              'await
          }
        }
      }
    }
    res match {
      case 'await => lock(requester)
      case 'retry => request(requester)
      case 'done =>
    }
  }

  /**
   * transfers the lock from the turn to the target.
   * this notifies all turns waiting on this lock because we need the turn the lock was transferred to to wake up
   */
  def transfer(target: Key, key: Key) = synchronized {
    if (!hasWriteAccess(key)) throw new IllegalMonitorStateException(s"$this is held by $owner but tried to transfer by $key (to $target)")
    owner = target
    if (target != null) target.addLock(this)
    notifyAll()
  }

}

