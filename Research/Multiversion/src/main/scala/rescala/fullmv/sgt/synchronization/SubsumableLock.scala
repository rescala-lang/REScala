package rescala.fullmv.sgt.synchronization

import rescala.parrp.Backoff

import scala.annotation.{elidable, tailrec}

trait SubsumableLock {
  @elidable(elidable.ASSERTION)
  private[synchronization] def isLockedRoot(key: Any): Boolean
  def getLockedRoot(key: Any): Option[SubsumableLock]
  def tryLock(key: Any): SubsumableLock.TryLockResult
  def lock(key: Any): SubsumableLock
  def unlock(key: Any): Unit
  protected def subsume(subsumableLock: SubsumableLock): SubsumableLock
}

object SubsumableLock {
  case class TryLockResult(success: Boolean, newRoot: SubsumableLock)

  def underLock[R](lock: SubsumableLock)(thunk: => R): R = {
    executeAndRelease(thunk, lock.lock(Thread.currentThread()))
  }
  def underLock[R](lockA: SubsumableLock, lockB: SubsumableLock)(thunk: => R): R = {
    executeAndRelease(thunk, lockAndMerge(Thread.currentThread(), lockA, lockB))
  }
  private def executeAndRelease[R](thunk: => R, locked: SubsumableLock): R = {
    try { thunk } finally {
      locked.unlock(Thread.currentThread())
    }
  }

  private def lockAndMerge(key: Any, lockA: SubsumableLock, lockB: SubsumableLock): SubsumableLock = {
//    lockAndMergeTryLockSpinOnly(key, lockA, lockB, new Backoff())
    lockAndMergeWithBackoff(key, lockA, lockB, new Backoff())
//    lockAndMergeWithoutBackoff(key, lockA, lockB)
  }

  @tailrec private def lockAndMergeTryLockSpinOnly(key: Any, lockA: SubsumableLock, lockB: SubsumableLock, backoff: Backoff): SubsumableLock = {
    val TryLockResult(successA, newRootA) = lockA.tryLock(key)
    if(!successA) {
      backoff.backoff()
      lockAndMergeTryLockSpinOnly(key, newRootA, lockB, backoff)
    } else {
      val TryLockResult(successB, newRootB) = lockB.tryLock(key)
      if(newRootA == newRootB) {
        assert(successB)
        newRootA
      } else if(successB) {
        newRootA.subsume(newRootB)
      } else {
        newRootA.unlock(key)
//        backoff.reset()
        backoff.backoff()
        lockAndMergeTryLockSpinOnly(key, newRootB, newRootA, backoff)
      }
    }
  }

  @tailrec private def lockAndMergeWithBackoff(key: Any, lockA: SubsumableLock, lockB: SubsumableLock, backoff: Backoff): SubsumableLock = {
    val lockedRootA = lockA.lock(key)
    val TryLockResult(successB, newRootB) = lockB.tryLock(key)
    if(lockedRootA == newRootB) {
      assert(successB)
      lockedRootA
    } else if(successB) {
      lockedRootA.subsume(newRootB)
    } else {
      lockedRootA.unlock(key)
      backoff.backoff()
      lockAndMergeWithBackoff(key, newRootB, lockedRootA, backoff)
    }
  }

  @tailrec private def lockAndMergeWithoutBackoff(key: Any, lockA: SubsumableLock, lockB: SubsumableLock): SubsumableLock = {
    val lockedRootA = lockA.lock(key)
    val TryLockResult(successB, newRootB) = lockB.tryLock(key)
    if(lockedRootA == newRootB) {
      assert(successB)
      lockedRootA
    } else if(successB) {
      lockedRootA.subsume(newRootB)
    } else {
      lockedRootA.unlock(key)
      lockAndMergeWithoutBackoff(key, newRootB, lockedRootA)
    }
  }
}
