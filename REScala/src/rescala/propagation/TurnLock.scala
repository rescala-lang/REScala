package rescala.propagation

import java.util.concurrent.locks.ReentrantLock

import scala.annotation.tailrec
import scala.annotation.tailrec

trait LockOwner {
  @volatile var request: Option[LockOwner] = None

  def grant(initial: LockOwner): Unit = {
    def run(other: LockOwner): Unit =
      other.request match {
        case None => other.request = Some(this)
        case Some(third) =>
          // should not deadlock, because everything else is either spinlocking, or locking in this same order here
          third.lock.lock()
          try run(third) finally third.lock.unlock()
      }
    run(initial)
  }

  val lock: ReentrantLock = new ReentrantLock()

  @volatile var heldLocks: List[TurnLock] = Nil

  def unlockAll() = heldLocks.distinct.foreach(_.unlock()(this))
  def transferAll(target: LockOwner)  = heldLocks.distinct.foreach(_.transfer(target)(this))

}

final class TurnLock {

  private var owner: LockOwner = null

  def owned(implicit turn: LockOwner): Boolean = synchronized(owner eq turn)

  def lock()(implicit turn: LockOwner): Unit = synchronized {
    while (!tryLock()) wait()
  }

  @tailrec
  def request()(implicit turn: LockOwner): Unit = {
    val done = synchronized {
      if (tryLock()) true
      else {
        tryLockAllOwners(turn, owner)(failureResult = false) {
          // test makes sure, that owner is not waiting on us
          if (!isShared()) turn.grant(owner)
          true
        }
      }
    }
    if (!done) request()
    else lock()
  }

  def isShared()(implicit turn: LockOwner): Boolean = synchronized {
    @tailrec
    def run(curr: LockOwner): Boolean =
      if (curr eq owner) true
      else curr.request match {
        case None => false
        case Some(req) => run(req)
      }
    run(turn)
  }

  private def tryLock()(implicit turn: LockOwner): Boolean = synchronized {
    if (owner eq null) {
      owner = turn
      turn.heldLocks ::= this
      true
    }
    else if (owned) true
    else false
  }

  def transfer(target: LockOwner)(implicit turn: LockOwner) = synchronized {
    if (owned) {
      owner = target
      notifyAll()
    }
    else throw new IllegalMonitorStateException(s"$this is held by $owner but tried to transfer by $turn (to $target)")
  }

  def unlock()(implicit turn: LockOwner): Unit = transfer(null)

  private def tryLockAllOwners[R](lo: LockOwner*)(failureResult: R)(f: => R): R = {
    val sorted = lo.sortBy(System.identityHashCode)
    val locked = sorted.takeWhile(_.lock.tryLock())
    try {
      if (locked.size == sorted.size) failureResult
      else f
    }
    finally locked.foreach(_.lock.unlock())
  }

}

