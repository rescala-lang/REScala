package rescala.propagation

import java.util.concurrent.locks.ReentrantLock

import scala.annotation.tailrec
import scala.annotation.tailrec

trait LockOwner {
  private implicit def currentLockOwner: LockOwner = this

  /** if we have a request from some other owner, that owner has given us shared access to all his locks
    * and is waiting for one of our locks to be transferred to him.
    * writing of this field is guarded by the masterLock */
  @volatile final var request: Option[LockOwner] = None

  /** this grants shared access to our locks to the group to which initial belongs.
    * when grant is called both masterLocks of us and initial must be held.
    * we then follow the request chain from initial until the end, locking everything along the way
    * (this should not deadlock, because the request chain gives a unique order to the locking process).
    * eventually when initial completes its turn, it will transfer all of its locks to us. */
  final def grant(initial: LockOwner): Unit = {
    def run(other: LockOwner): Unit =
      other.request match {
        case None => other.request = Some(this)
        case Some(third) =>
          // should not deadlock, because everything else is either spinlocking, or locking in this same order here
          third.withMaster(run(third))
      }
    run(initial)
  }

  /** the master lock guards writes to the requester, as well as all unlocks
    * also this lock will be held when a turn request the locks of another
    * this prevents a cycle of turns to lock each other and create a ring of waiting turns */
  final val masterLock: ReentrantLock = new ReentrantLock()
  def withMaster[R](f: => R): R = {
    masterLock.lock()
    try f
    finally masterLock.unlock()
  }

  /** contains a list of all locks owned by us.
    * this does not need synchronisation because it is only written in 2 cases:
    * 1: when the current transaction locks something
    * 2: when the transaction we are waiting for transfers their locks
    * these two things are mutually exclusive. */
  @volatile private var heldLocks: List[TurnLock] = Nil

  final def addLock(lock: TurnLock): Unit = heldLocks ::= lock

  /** both unlock and transfer assume that the master lock is locked*/
  private def unlockAll(): Unit = heldLocks.distinct.foreach(_.unlock()(this))
  /** we acquire the master lock for the target, because the target waits on one of the locks we transfer,
    * and it will wake up as soon as that one is unlocked and we do not want the target to start unlocking
    * or wait on someone else before we have everything transferred */
  private def transferAll(target: LockOwner): Unit = target.withMaster {
    target.heldLocks :::= heldLocks
    heldLocks.distinct.foreach(_.transfer(target)(this))
  }

  /** release all locks we hold or transfer them to a waiting transaction if there is one
    * holds the master lock for request */
  final def releaseAll(): Unit = withMaster {
    request match {
      case Some(req) =>
        transferAll(req)
      case None =>
        unlockAll()
    }
  }


}

final class TurnLock {

  /** this is guarded by our intrinsic lock */
  private var owner: LockOwner = null

  def isOwned(implicit turn: LockOwner): Boolean = synchronized(owner eq turn)

  /** accessible effectively means that we can do whatever with the locked object */
  def isAccessible(implicit turn: LockOwner): Boolean = synchronized(isOwned || isShared)

  /** this will block until the lock is owned by the turn.
    * this does not dest for shared access and thus will deadlock if the current owner has its locks shared with the turn */
  def lock()(implicit turn: LockOwner): Unit = synchronized {
    while (!tryLock()) wait()
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
  def request()(implicit turn: LockOwner): Unit = {
    synchronized {
      if (tryLock()) 'done
      else {
        tryLockAllOwners(turn, owner)(failureResult = 'retry) {
          // test makes sure, that owner is not waiting on us
          if (isShared) 'done
          else {
            turn.grant(owner)
            'await
          }
        }
      }
    } match {
      case 'await => lock()
      case 'retry => request()
      case 'done =>
    }
  }


  /** traverses the request queue starting from the turn and checks if any of the waiting turns owns this lock  */
  def isShared(implicit turn: LockOwner): Boolean = synchronized {
    @tailrec
    def run(curr: LockOwner): Boolean =
      if (curr eq owner) true
      else curr.request match {
        case None => false
        case Some(req) => run(req)
      }
    run(turn)
  }

  /** locks this if it is free, returns true if the turn owns this lock.
    * does not check for shared access. */
  private def tryLock()(implicit turn: LockOwner): Boolean = synchronized {
    if (owner eq null) {
      owner = turn
      turn.addLock(this)
      true
    }
    else if (isOwned) true
    else false
  }

  /** transfers the lock from the turn to the target.
    * this notifies all turns waiting on this lock because we need the turn the lock was transferred to to wake up
    * (it will currently be waiting in the lock call made at the end of request */
  def transfer(target: LockOwner)(implicit turn: LockOwner) = synchronized {
    if (isOwned) {
      owner = target
      notifyAll()
    }
    else throw new IllegalMonitorStateException(s"$this is held by $owner but tried to transfer by $turn (to $target)")
  }

  /** transferring to null frees the owner */
  def unlock()(implicit turn: LockOwner): Unit = transfer(null)

  /** this tries to get all master locks of the given owners in a fixed order.
    * it returns the failure value if it could not acquire all locks,
    * or execute the handler with all locks held if it could */
  private def tryLockAllOwners[R](lo: LockOwner*)(failureResult: R)(f: => R): R = {
    val sorted = lo.sortBy(System.identityHashCode)
    val locked = sorted.takeWhile(_.masterLock.tryLock())
    try {
      if (locked.size == sorted.size) failureResult
      else f
    }
    finally locked.foreach(_.masterLock.unlock())
  }

}

