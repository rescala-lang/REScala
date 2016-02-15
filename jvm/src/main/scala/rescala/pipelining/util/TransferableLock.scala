package rescala.pipelining.util

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.LockSupport

class TransferableLock {

  private object heldLock
  private var heldBy = null.asInstanceOf[Thread]
  private var onlyAllowed = null.asInstanceOf[Thread]
  private var waitingThreads = Set[Thread]()

  def lock() = {
    val thread = Thread.currentThread()
    var isLockedByOther = true
    while (isLockedByOther) {
      val lockedBy = heldLock.synchronized {
        if (heldBy == null && (onlyAllowed == thread || onlyAllowed == null)) {
          heldBy = thread
        } else {
          waitingThreads += thread
        }
        heldBy
      }
      isLockedByOther = !(lockedBy eq thread)
      if (isLockedByOther) {
        LockSupport.park(lockedBy)
      }
    }
  }

  def reserveLockFor(to: Thread) = {
    val thread = Thread.currentThread()
    heldLock.synchronized {
      if (!(heldBy == null || heldBy == thread))
        throw new IllegalMonitorStateException("The lock can only be reserved if it is not owned or owned by the current thread")

      if (heldBy == null || !waitingThreads.contains(to)) {
        onlyAllowed = to
        heldBy = null
      } else {
        transfer(to)
      }
    }
  }

  def transfer(to: Thread) = {
    val thread = Thread.currentThread()
    val threadsToWake = heldLock.synchronized {
      if (heldBy != thread)
        throw new IllegalMonitorStateException("Lock is transfered by a thread which does not own it")
      heldBy = null
      if (to != null) {
        assert(waitingThreads.contains(to), "Cannot transfert the lock to a thread which does not acquires it")
        waitingThreads -= to
        onlyAllowed = to
        Set(to)
      } else {
        onlyAllowed = null
        val threads = waitingThreads
        waitingThreads = Set()
        threads
      }

    }
    threadsToWake.foreach { LockSupport.unpark(_) }
  }

  def unlock() = {
    transfer(null)
  }

}