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
  def spinOnce(backoff: Long): SubsumableLock.TryLockResult
  def subsume(lockedNewParent: SubsumableLock.TryLockResult): Unit
  // if failed, returns Some(newParent) that should be used as new Parent
  // if successful, returns None; lockedNewParent.newParent should be used as newParent.
  // newParent is not a uniform part of all possible return values to avoid establishing unnecessary back-and-forth remote paths
  def trySubsume(lockedNewParent: SubsumableLock.TryLockResult): Option[SubsumableLock]
}

object SubsumableLock {
  val DEBUG = false
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
      val resB = lockB.trySubsume(resA)
      if(resB.isEmpty) {
        resA
      } else {
        resA.newParent.unlock()
//        backoff.reset()
        backoff.backoff()
        lockAndMergeTryLockSpinOnly(resB.get, resA.newParent, backoff)
      }
    }
  }

  @tailrec private def lockAndMergeWithBackoff(lockA: SubsumableLock, lockB: SubsumableLock, backoff: Backoff): TryLockResult = {
    val resA = lockA.lock()
    val resB = lockB.trySubsume(resA)
    if(resB.isEmpty) {
      resA
    } else {
      resA.newParent.unlock()
      backoff.backoff()
      lockAndMergeWithBackoff(resB.get, resA.newParent, backoff)
    }
  }

  @tailrec private def lockAndMergeWithoutBackoff(lockA: SubsumableLock, lockB: SubsumableLock): TryLockResult = {
    if(DEBUG) System.out.println(s"[${Thread.currentThread().getName}] syncing $lockA and $lockB")
    val resA = lockA.lock()
    val resB = lockB.trySubsume(resA)
    if(resB.isEmpty) {
      resA
    } else {
      resA.newParent.unlock()
      lockAndMergeWithoutBackoff(resB.get, resA.newParent)
    }
  }
}
