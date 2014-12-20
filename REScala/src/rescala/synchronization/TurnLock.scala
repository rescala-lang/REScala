package rescala.synchronization

import java.util.concurrent.{ConcurrentSkipListMap, ConcurrentHashMap}
import scala.collection.JavaConverters.mapAsScalaMapConverter

import rescala.graph.Reactive

import scala.annotation.tailrec

final class TurnLock(val guarded: Reactive) {

  override def toString: String = s"Lock($guarded)"

  /** this is guarded by our intrinsic lock */
  private var owner: Key = null

  val wantThis = new ConcurrentHashMap[Key, None.type]()
  
  def wantedBy(key: Key): Unit = wantThis.put(key, None)

  def getOwner: Key = synchronized(owner)

  /** returns true if key owns the write lock */
  def hasWriteAccess(key: Key): Boolean = synchronized(owner eq key)

  /**
   * acquires dynamic acces to the lock.
   * this can block until all other turns waiting on the lock have finished
   */
  def acquireDynamic(key: Key): Unit = request(key)(SyncUtil.Done(Unit)) { _ =>
    key.appendAfter(owner)
    SyncUtil.Await
  }

  /**
   * this will block until the lock is owned by the turn.
   * this does not test for shared access and thus will deadlock if the current owner has its locks shared with the turn.
   * use with caution as this can potentially deadlock
   */
  def lock(key: Key): Unit = {
    synchronized { while (tryLock(key) ne key) wait() }
    // wait for master lock to become free
    key.synchronized(Unit)
  }

    /**
   * locks this if it is free, returns the current owner (which is key, if locking succeeded)
   * does not check for shared access.
   */
  def tryLock(key: Key, register: Boolean = true): Key = synchronized {
    if (owner eq null) {
      owner = key
      wantThis.remove(key, None)
      key.addLock(this)
    }
    else if (register) {
      wantedBy(key)
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
  def transfer(target: Key, oldOwner: Key, wantBack: Boolean) = synchronized {
    if (!hasWriteAccess(oldOwner)) throw new IllegalMonitorStateException(s"$this is held by $owner but tried to transfer by $oldOwner (to $target)")

    if (wantBack) wantedBy(oldOwner)
    else {
      wantThis.remove(oldOwner, None)
      //assert(!wantThis.containsKey(oldOwner), s"$oldOwner gave $this away without wanting it back, but wanted by ${wantThis.asScala.keySet}")
    }

    if (wantThis.isEmpty) owner = null
    else if (target != null) {
      owner = target
      wantThis.remove(target, None)
      target.addLock(this)
    }
    else {
      val waiting = wantThis.keys().nextElement()
      assert(wantThis.remove(waiting, None), s"got $waiting out of wantThis but was not there anymore!")
      owner = waiting
      waiting.addLock(this)
    }
    notifyAll()
  }

}

