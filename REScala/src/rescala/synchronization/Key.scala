package rescala.synchronization

import java.util.concurrent.ConcurrentLinkedQueue

import rescala.graph.Globals
import rescala.turns.Turn

import scala.annotation.tailrec

final class Key(val turn: Turn) {
  val id = Globals.nextID()
  override def toString: String = s"Key($id)"

  /** if we have a request from some other owner, that owner has given us shared access to all his locks
    * and is waiting for one of our locks to be transferred to him.
    * writing of this field is guarded by our intrinsic lock */
  @volatile var subsequent: Option[Key] = None
  @volatile var prior: Option[Key] = None

  /** contains a list of all locks owned by us. */
  private[this] val heldLocks = new ConcurrentLinkedQueue[TurnLock]()

  def addLock(lock: TurnLock): Unit = {
    heldLocks.add(lock)
  }

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
  def transferAll(target: Key): Unit =
    while (!heldLocks.isEmpty) {
      val head = heldLocks.poll()
      val owner = head.getOwner
      if (owner eq this) {
        head.transfer(target, this)
      }
      else assert(owner eq target, s"transfer of $head from $this to $target failed, because it was owned by $owner")
    }

  /** release all locks we hold or transfer them to a waiting transaction if there is one
    * holds the master lock for request */
  def releaseAll(): Unit =
    synchronized {
      subsequent match {
        case Some(subseq) => subseq.synchronized {
          subseq.prior = None
          subsequent = None
          transferAll(subseq)
        }
        case None =>
          transferAll(null)
      }
    }


  @tailrec
  def controls(target: Key): Boolean =
    if (this eq target) true
    else subsequent match {
      case None => false
      case Some(next) => next.controls(target)
    }


}
