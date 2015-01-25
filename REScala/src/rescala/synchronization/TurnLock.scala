package rescala.synchronization

import rescala.graph.Reactive
import rescala.synchronization.Keychains.{Await, Done}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

final class TurnLock(val guarded: Reactive) {

  override def toString: String = s"Lock($guarded)"

  /** this is guarded by our intrinsic lock */
  private var owner: Key = null
  private var shared: Queue[Key] = Queue()

  def getOwner: Key = synchronized(owner)

  /** returns true if key owns the write lock */
  def isOwner(key: Key): Boolean = synchronized(owner eq key)

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
  def acquireShared(requester: Key): Key = {
    val oldOwner = tryLock(requester)

    val res =
      if (oldOwner eq requester) Keychains.Done(requester)
      else {
        Keychains.lockKeychains(requester, oldOwner) {
          synchronized {
            tryLock(requester) match {
              // make sure the other owner did not unlock before we got his master lock
              case _ if owner eq requester => Keychains.Done(requester)
              case _ if owner ne oldOwner => Keychains.Retry
              case _ if requester.keychain eq owner.keychain => Done(owner)
              case _ =>
                shared = shared.enqueue(requester)
                owner.keychain.append(requester.keychain)
                Await
            }
          }
        }
      }
    res match {
      case Keychains.Await =>
        Keychains.await(requester)
        synchronized {
          val (k, r) = shared.dequeue
          assert(k == requester, s"resolved await in wrong order got $k expected $requester remaining $r")
          shared = r
          requester
        }
      case Keychains.Retry => acquireShared(requester)
      case Keychains.Done(o) => o
    }
  }

  /** transfers the lock from the turn to the target. */
  def transfer(target: Key, oldOwner: Key) = synchronized {
    assert(owner eq oldOwner, s"$this is held by $owner but tried to transfer by $oldOwner (to $target)")
    if (shared.isEmpty) owner = null
    else {
      owner = target
      if (target ne null) target.addLock(this)
    }
  }

}

