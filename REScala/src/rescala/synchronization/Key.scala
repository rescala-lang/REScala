package rescala.synchronization

import java.util.concurrent.locks.{Condition, ReentrantLock}

import rescala.graph.Reactive

final class Key(val handleDependencyChange: (Reactive, Reactive) => Unit) {

  val id = SyncUtil.counter.getAndIncrement
  override def toString: String = s"Key($id)"

  /** if we have a request from some other owner, that owner has given us shared access to all his locks
    * and is waiting for one of our locks to be transferred to him.
    * writing of this field is guarded by the masterLock */
  @volatile var subsequent: Option[Key] = None


  /** the master lock guards writes to the requester, as well as all unlocks
    * also this lock will be held when a turn request the locks of another
    * this prevents a cycle of turns to lock each other and create a ring of waiting turns */
  val keyLock: ReentrantLock = new ReentrantLock()
  def withMaster[R](f: => R): R = {
    keyLock.lock()
    try f
    finally keyLock.unlock()
  }

  /** contains a list of all locks owned by us.
    * this does not need synchronisation because it is only written in 2 cases:
    * 1: when the current transaction locks something
    * 2: when the transaction we are waiting for transfers their locks
    * these two things are mutually exclusive.
    * we might even get away without the volatile, because the wait/notify creates a happen before relationship
    * but we will still keep it, because concurrency is scary */
  @volatile private[synchronization] var heldLocks: List[TurnLock] = Nil

  def addLock(lock: TurnLock): Unit = heldLocks ::= lock

  /** this grants shared access to our locks to the group to which initial belongs.
    * when grant is called both masterLocks of us and target must be held.
    * we then follow the request chain from initial until the end, locking everything along the way
    * (this should not deadlock, because the request chain gives a unique order to the locking process).
    * eventually when target and its wait chain complete their turns, they will transfer all of their locks to us.
    * returns the actual key we ended up waiting for */
  def append(target: Key): Key =
    target.subsequent match {
      case None =>
        target.subsequent = Some(this)
        target
      case Some(third) =>
        // should not deadlock, because everything else is locking in this same order here
        third.withMaster(append(third))
    }

  /** both unlock and transfer assume that the master lock is locked */
  private def unlockAll(): Unit = heldLocks.distinct.foreach(_.unlock(this))

  /** we acquire the master lock for the target, because the target waits on one of the locks we transfer,
    * and it will wake up as soon as that one is unlocked and we do not want the target to start unlocking
    * or wait on someone else before we have everything transferred */
  private def transferAll(target: Key): Unit = target.withMaster {
    val distinc = heldLocks.distinct
    target.heldLocks :::= distinc
    distinc.foreach(_.transfer(target)(this))
  }

  /** release all locks we hold or transfer them to a waiting transaction if there is one
    * holds the master lock for request */
  def releaseAll(): Unit = withMaster {
    subsequent match {
      case Some(req) =>
        transferAll(req)
      case None =>
        unlockAll()
    }
    heldLocks = Nil
    subsequent = None
  }


}
