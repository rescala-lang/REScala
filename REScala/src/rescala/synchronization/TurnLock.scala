package rescala.synchronization

import rescala.graph.Reactive

import scala.annotation.tailrec


final class TurnLock(val guards: Reactive) {

  override def toString: String = s"Lock($guards)"

  /** this is guarded by our intrinsic lock */
  private var owner: Key = null

  def getOwner: Key = synchronized(owner)

  /** returns true if key own the write lock */
  def hasWriteAccess(key: Key): Boolean = synchronized(owner eq key)

  /** accessible effectively means that we are allowed to read the locked object and write its sinks */
  def hasReadAccess(key: Key): Boolean = synchronized(hasWriteAccess(key) || isShared(key))

  /** this will block until the lock is owned by the turn.
    * this does not dest for shared access and thus will deadlock if the current owner has its locks shared with the turn */
  def lock(turn: Key): Unit = synchronized {
    while (tryLock(turn) ne turn) wait()
  }

  /** locks this if it is free, returns true if the turn owns this lock.
    * does not check for shared access. */
  private def tryLock(turn: Key): Key = synchronized {
    if (owner eq null) {
      owner = turn
      turn.addLock(this)
    }
    owner
  }

  /** request basically means that the turn will share all its locks with the owner of the current lock
    * and in exchange request that the owner will transfer all of its locks to the turn when he is finished.
    *
    * this will always first try to normally lock if that fails it will acquire the master locks for the turn and the owner.
    * the locks are acquired in order as to prevent deadlocks if some cycle of turns wants to lock each other
    * this spinlocks (incuding our intrinsic lock), so that the intrinsic lock is not blocked while we wait for the master locks.
    * if we have both locks we check if the owner has shared his locks with us so that we do not request in cycles
    * and then grant the owner access to our locks (which will cause him to transfer locks to us when he unlocks)
    *
    * as we hold all the master locks, the owner can not be in the middle of unlocking stuff, so we do always get everything.
    * */
  @tailrec
  def request(requester: Key): Unit = {
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
            case _ if isShared(requester) => 'done
            // trade our rights
            case _ =>
              requester.append(owner)
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


  /** traverses the request queue starting from the turn and checks if any of the waiting turns owns this lock  */
  def isShared(turn: Key): Boolean = synchronized {
    @tailrec
    def run(curr: Key): Boolean =
      if (curr eq owner) true
      else curr.subsequent match {
        case None => false
        case Some(req) => run(req)
      }
    run(turn)
  }

  /** transfers the lock from the turn to the target.
    * this notifies all turns waiting on this lock because we need the turn the lock was transferred to to wake up
    * (it will currently be waiting in the lock call made at the end of request */
  def transfer(target: Key)(turn: Key) = synchronized {
    if (hasWriteAccess(turn)) {
      owner = target
      notifyAll()
    }
    else throw new IllegalMonitorStateException(s"$this is held by $owner but tried to transfer by $turn (to $target)")
  }

  /** transferring to null frees the owner */
  def unlock(turn: Key): Unit = transfer(null)(turn)


}

