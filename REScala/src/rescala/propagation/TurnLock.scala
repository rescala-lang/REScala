package rescala.propagation

import java.util.concurrent.locks.{AbstractQueuedSynchronizer, ReentrantLock}

import rescala.propagation.turns.Pessimistic

import scala.annotation.tailrec

trait LockOwner {
  @volatile var request: Option[LockOwner] = None

  def grant(other: LockOwner): Unit = {
    if (other.request.isDefined) throw new NotImplementedError(s"handling of owner chains missing")
    other.request = Some(this)
  }

  val lock: ReentrantLock = new ReentrantLock()

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
        tryLockAll(turn, owner)(failureResult = false) {
          turn.grant(owner)
          true
        }
      }
    }
    if (!done) request()
    else lock()
  }

  def sharedLock()(implicit turn: LockOwner): Boolean = synchronized { turn.request == Some(owner) }

  private def tryLock()(implicit turn: LockOwner): Boolean = synchronized {
    if (owner eq null) {
      owner = turn
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

  private def tryLockAll[R](lo: LockOwner*)(failureResult: R)(f: => R): R = {
    val sorted = lo.sortBy(System.identityHashCode)
    val locked = sorted.takeWhile(_.lock.tryLock())
    try {
      if (locked.size == sorted.size) failureResult
      else f
    }
    finally locked.foreach(_.lock.unlock())
  }

}

