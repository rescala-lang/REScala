package rescala.propagation

import java.util.concurrent.locks.ReentrantLock

import scala.annotation.tailrec
import scala.annotation.tailrec

trait LockOwner {
  private implicit def currentLockOwner: LockOwner = this

  /** if we have a request from some other owner, that owner has given us shared access to all his locks */
  @volatile final var request: Option[LockOwner] = None

  final def grant(initial: LockOwner): Unit = {
    def run(other: LockOwner): Unit =
      other.request match {
        case None => other.request = Some(this)
        case Some(third) =>
          // should not deadlock, because everything else is either spinlocking, or locking in this same order here
          third.masterLock.lock()
          try run(third) finally third.masterLock.unlock()
      }
    run(initial)
  }

  final val masterLock: ReentrantLock = new ReentrantLock()

  @volatile final var heldLocks: List[TurnLock] = Nil

  final def addLock(lock: TurnLock) = heldLocks ::= lock

  private def unlockAll() = heldLocks.distinct.foreach(_.unlock()(this))
  private def transferAll(target: LockOwner)  = heldLocks.distinct.foreach(_.transfer(target)(this))

  final def lockOrdered(locks: Seq[TurnLock]): Unit = locks.sortBy(System.identityHashCode).foreach{ l => l.lock() }

  final def releaseAll(): Unit = {
    masterLock.lock()
    try request match {
      case Some(req) =>
        transferAll(req)
      case None =>
        unlockAll()
    }
    finally {
      masterLock.unlock()
    }
  }

}

final class TurnLock {

  private var owner: LockOwner = null

  def isOwned(implicit turn: LockOwner): Boolean = synchronized(owner eq turn)

  def isAccessible(implicit turn: LockOwner): Boolean = synchronized(isOwned || isShared)

  def lock()(implicit turn: LockOwner): Unit = synchronized {
    while (!tryLock()) wait()
  }

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

  private def tryLock()(implicit turn: LockOwner): Boolean = synchronized {
    if (owner eq null) {
      owner = turn
      turn.addLock(this)
      true
    }
    else if (isOwned) true
    else false
  }

  def transfer(target: LockOwner)(implicit turn: LockOwner) = synchronized {
    if (isOwned) {
      owner = target
      notifyAll()
    }
    else throw new IllegalMonitorStateException(s"$this is held by $owner but tried to transfer by $turn (to $target)")
  }

  def unlock()(implicit turn: LockOwner): Unit = transfer(null)

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

