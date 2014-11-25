package rescala.propagation.turns

import java.util.concurrent.locks.ReentrantLock

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
  @volatile final protected var heldLocks: List[TurnLock] = Nil

  final def addLock(lock: TurnLock): Unit = heldLocks ::= lock

  /** both unlock and transfer assume that the master lock is locked */
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
