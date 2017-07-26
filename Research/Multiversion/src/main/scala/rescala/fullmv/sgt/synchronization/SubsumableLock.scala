package rescala.fullmv.sgt.synchronization

import rescala.parrp.Backoff

import scala.annotation.tailrec

// TODO must be a remote interface
trait SubsumableLock {
  // used for assertions only
  def getLockedRoot: Option[SubsumableLock]
  def tryLock(): SubsumableLock.TryLockResult
  def lock(): SubsumableLock
  def unlock(): Unit
  def subsume(subsumableLock: SubsumableLock): SubsumableLock
}

object SubsumableLock {
  case class TryLockResult(success: Boolean, newRoot: SubsumableLock)

  def underLock[R](lock: SubsumableLock)(thunk: => R): R = {
    executeAndRelease(thunk, lock.lock())
  }
  def underLock[R](lockA: SubsumableLock, lockB: SubsumableLock)(thunk: => R): R = {
    executeAndRelease(thunk, lockAndMerge(lockA, lockB))
  }
  private def executeAndRelease[R](thunk: => R, locked: SubsumableLock): R = {
    try { thunk } finally {
      locked.unlock()
    }
  }

  private def lockAndMerge(lockA: SubsumableLock, lockB: SubsumableLock): SubsumableLock = {
//    lockAndMergeTryLockSpinOnly(lockA, lockB, new Backoff())
//    lockAndMergeWithBackoff(lockA, lockB, new Backoff())
    lockAndMergeWithoutBackoff(lockA, lockB)
  }

  @tailrec private def lockAndMergeTryLockSpinOnly(lockA: SubsumableLock, lockB: SubsumableLock, backoff: Backoff): SubsumableLock = {
    val TryLockResult(successA, newRootA) = lockA.tryLock()
    if(!successA) {
      backoff.backoff()
      lockAndMergeTryLockSpinOnly(newRootA, lockB, backoff)
    } else {
      val TryLockResult(successB, newRootB) = lockB.tryLock()
      if(newRootA == newRootB) {
        newRootA
      } else if(successB) {
        newRootA.subsume(newRootB)
      } else {
        newRootA.unlock()
//        backoff.reset()
        backoff.backoff()
        lockAndMergeTryLockSpinOnly(newRootB, newRootA, backoff)
      }
    }
  }

  @tailrec private def lockAndMergeWithBackoff(lockA: SubsumableLock, lockB: SubsumableLock, backoff: Backoff): SubsumableLock = {
    val lockedRootA = lockA.lock()
    val TryLockResult(successB, newRootB) = lockB.tryLock()
    if(lockedRootA == newRootB) {
      lockedRootA
    } else if(successB) {
      lockedRootA.subsume(newRootB)
    } else {
      lockedRootA.unlock()
      backoff.backoff()
      lockAndMergeWithBackoff(newRootB, lockedRootA, backoff)
    }
  }

  @tailrec private def lockAndMergeWithoutBackoff(lockA: SubsumableLock, lockB: SubsumableLock): SubsumableLock = {
    val lockedRootA = lockA.lock()
    val TryLockResult(successB, newRootB) = lockB.tryLock()
    if(lockedRootA == newRootB) {
      lockedRootA
    } else if(successB) {
      lockedRootA.subsume(newRootB)
    } else {
      lockedRootA.unlock()
      lockAndMergeWithoutBackoff(newRootB, lockedRootA)
    }
  }
}
