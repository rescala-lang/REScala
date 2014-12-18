package rescala.synchronization

import java.util.concurrent.locks.ReentrantLock

import rescala.graph.Globals
import rescala.turns.Turn

import scala.annotation.tailrec

final class Key(val turn: Turn) {
  val id = Globals.nextID()
  override def toString: String = s"Key($id)"

  /** if we have a request from some other owner, that owner has given us shared access to all his locks
    * and is waiting for one of our locks to be transferred to him.
    * writing of this field is guarded by the masterLock */
  @volatile var subsequent: Option[Key] = None
  @volatile var prior: Option[Key] = None


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
  @volatile private[this] var heldLocks: List[TurnLock] = Nil

  def addLock(lock: TurnLock): Unit = synchronized { heldLocks ::= lock }

  /** this grants shared access to our locks to the group to which initial belongs.
    * when grant is called both masterLocks of us and target must be held.
    * we then follow the request chain from initial until the end, locking everything along the way
    * (this should not deadlock, because the request chain gives a unique order to the locking process).
    * eventually when target and its wait chain complete their turns, they will transfer all of their locks to us.
    * returns the actual key we ended up waiting for */
  @tailrec
  def appendAfter(target: Key): Key =
    target.subsequent match {
      case None =>
        target.subsequent = Some(this)
        this.prior = Some(target)
        target
      case Some(third) =>
        // should not deadlock, because everything else is locking in this same order here
        appendAfter(third)
    }

  /** we acquire the master lock for the target, because the target waits on one of the locks we transfer,
    * and it will wake up as soon as that one is unlocked and we do not want the target to start unlocking
    * or wait on someone else before we have everything transferred */
  def transferAll(target: Key): Unit = {
    synchronized {
      heldLocks.foreach { l =>
        val owner = l.getOwner
        if (owner eq this) l.transfer(target, this)
        else assert(owner eq target, s"transfer of $l from $this to $target failed, becaus it was owned by $owner")
      }
      heldLocks = Nil
    }
  }

  /** release all locks we hold or transfer them to a waiting transaction if there is one
    * holds the master lock for request */
  def releaseAll(): Unit = withMaster {
    synchronized {
      subsequent match {
        case Some(req) => req.withMaster {
          subsequent = None
          req.prior = None
          transferAll(req)
        }
        case None =>
          transferAll(null)
      }
    }
  }


}
