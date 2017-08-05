package rescala.fullmv.sgt.synchronization

import rescala.parrp.Backoff

import scala.annotation.tailrec

// TODO must be a remote interface
trait SubsumableLock {
  // used for assertions only
  def getLockedRoot: Option[SubsumableLock.GUID]
  def tryLock(): SubsumableLock.TryLockResult
  def lock(): SubsumableLock.TryLockResult
  def unlock(): Unit
  def subsume(subsumableLock: SubsumableLock.TryLockResult): SubsumableLock.TryLockResult
}

object SubsumableLock {
  type GUID = Long
  case class TryLockResult(success: Boolean, newParent: SubsumableLock, globalRoot: GUID)

  def underLock[R](lock: SubsumableLock)(thunk: => R): R = {
    executeAndRelease(thunk, lock.lock().newParent)
  }
  def underLock[R](lockA: SubsumableLock, lockB: SubsumableLock)(thunk: => R): R = {
    executeAndRelease(thunk, lockAndMerge(lockA, lockB).newParent)
  }
  private def executeAndRelease[R](thunk: => R, locked: SubsumableLock): R = {
    try { thunk } finally {
      locked.unlock()
    }
  }

  private def lockAndMerge(lockA: SubsumableLock, lockB: SubsumableLock): TryLockResult = {
//    lockAndMergeTryLockSpinOnly(lockA, lockB, new Backoff())
//    lockAndMergeWithBackoff(lockA, lockB, new Backoff())
    lockAndMergeWithoutBackoff(lockA, lockB)
  }

  @tailrec private def lockAndMergeTryLockSpinOnly(lockA: SubsumableLock, lockB: SubsumableLock, backoff: Backoff): TryLockResult = {
    val resA = lockA.tryLock()
    if(!resA.success) {
      backoff.backoff()
      lockAndMergeTryLockSpinOnly(resA.newParent, lockB, backoff)
    } else {
      val resB = lockB.tryLock()
      if(resA.globalRoot == resB.globalRoot) {
        resA
      } else if(resB.success) {
        resA.newParent.subsume(resB)
      } else {
        resA.newParent.unlock()
//        backoff.reset()
        backoff.backoff()
        lockAndMergeTryLockSpinOnly(resB.newParent, resA.newParent, backoff)
      }
    }
  }

  @tailrec private def lockAndMergeWithBackoff(lockA: SubsumableLock, lockB: SubsumableLock, backoff: Backoff): TryLockResult = {
    val resA = lockA.lock()
    val resB = lockB.tryLock()
    if(resA.globalRoot == resB.globalRoot) {
      resA
    } else if(resB.success) {
      resA.newParent.subsume(resB)
    } else {
      resA.newParent.unlock()
      backoff.backoff()
      lockAndMergeWithBackoff(resB.newParent, resA.newParent, backoff)
    }
  }

  @tailrec private def lockAndMergeWithoutBackoff(lockA: SubsumableLock, lockB: SubsumableLock): TryLockResult = {
    val resA = lockA.lock()
    val resB = lockB.tryLock()
    if(resA.globalRoot == resB.globalRoot) {
      resA
    } else if(resB.success) {
      resA.newParent.subsume(resB)
    } else {
      resA.newParent.unlock()
      lockAndMergeWithoutBackoff(resB.newParent, resA.newParent)
    }
  }
}
