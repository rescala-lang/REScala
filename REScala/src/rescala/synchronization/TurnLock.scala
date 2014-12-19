package rescala.synchronization

import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters.mapAsScalaMapConverter

import rescala.graph.Reactive

import scala.annotation.tailrec

final class TurnLock(val guarded: Reactive) {

  override def toString: String = s"Lock($guarded)"

  /** this is guarded by our intrinsic lock */
  private var owner: Key = null

  val wantThis = new ConcurrentHashMap[Key, None.type]()
  
  def wantedBy(key: Key): Unit = {
    assert(wantThis.put(key, None) == null, s"$key wanted $this twice")
  }

  def getOwner: Key = synchronized(owner)

  /** returns true if key owns the write lock */
  def hasWriteAccess(key: Key): Boolean = synchronized(owner eq key)

  /**
   * acquires dynamic acces to the lock.
   * this can block until all other turns waiting on the lock have finished
   */
  def acquireDynamic(key: Key): Unit = request(key)(SyncUtil.Done(Unit)) { _ =>
    wantedBy(key)
    key.appendAfter(owner)
    SyncUtil.Await
  }

  /**
   * this will block until the lock is owned by the turn.
   * this does not test for shared access and thus will deadlock if the current owner has its locks shared with the turn.
   * use with caution as this can potentially deadlock
   */
  def lock(key: Key): Unit = {
    synchronized {
      while (tryLock(key) ne key) {
        assert(wantThis.containsKey(key), s"$key waits without wanting $this")
        wait()
      }
    }
    // wait for master lock to become free
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

  /** request tries to */
  @tailrec
  def request(requester: Key)(waiting: => SyncUtil.Result[Unit])(other: Key => SyncUtil.Result[Unit]): Unit = {
    val oldOwner = tryLock(requester)
    val res = if (oldOwner eq requester) SyncUtil.Done(Unit)
    else {
      SyncUtil.lockLanes(requester, oldOwner) { ownerHead =>
        synchronized {
          tryLock(requester) match {
            // make sure the other owner did not unlock before we got his master lock
            case _ if owner eq requester => SyncUtil.Done(Unit)
            case _ if owner ne oldOwner => SyncUtil.Retry
            case _ if requester.controls(owner) => waiting
            case _ => other(ownerHead)
          }
        }
      }
    }
    res match {
      case SyncUtil.Await => lock(requester)
      case SyncUtil.Retry => request(requester)(waiting)(other)
      case SyncUtil.Done(_) =>
    }
  }

  /**
   * transfers the lock from the turn to the target.
   * this notifies all turns waiting on this lock because we need the turn the lock was transferred to to wake up
   */
  def transfer(target: Key, oldOwner: Key) = synchronized {
    if (!hasWriteAccess(oldOwner)) throw new IllegalMonitorStateException(s"$this is held by $owner but tried to transfer by $oldOwner (to $target)")
    if (wantThis.isEmpty) owner = null
    else {
      owner = target
      if (target != null) {
        assert(target.waitingList().::(oldOwner).exists(wantThis.containsKey),
          s"$oldOwner tries to transfer $this to $target but only ${wantThis.asScala.keySet} want it, not: ${target.waitingList()}")
        target.addLock(this)
      }
      else assert(false, s"unlocked lock wanted by ${wantThis.size()}")

    }
    notifyAll()
  }

}

