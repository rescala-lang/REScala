package rescala.fullmv.sgt.synchronization

import rescala.fullmv.mirrors.{Host, Hosted, SubsumableLockHost}
import rescala.parrp.Backoff

import scala.annotation.tailrec

trait SubsumableLockEntryPoints extends Hosted {
  // used for assertions only
  def getLockedRoot: Option[Host.GUID]
  def tryLock(): SubsumableLock.TryLockResult
  def lock(): SubsumableLock.TryLockResult
  def spinOnce(backoff: Long): SubsumableLock.TryLockResult
  // if failed, returns Some(newParent) that should be used as new Parent
  // if successful, returns None; lockedNewParent.newParent should be used as newParent.
  // newParent is not a uniform part of all possible return values to avoid establishing unnecessary back-and-forth remote paths
  def trySubsume(lockedNewParent: SubsumableLock.TryLockResult): Option[SubsumableLock]
}

trait SubsumableLock extends SubsumableLockEntryPoints {
  override val host: SubsumableLockHost
  def subsume(lockedNewParent: SubsumableLock.TryLockResult): Unit
  def unlock(): Unit
}

object SubsumableLock {
  val DEBUG = false
  case class TryLockResult(success: Boolean, newParent: SubsumableLock, globalRoot: Host.GUID)

  def underLock[R](lock: SubsumableLockEntryPoints)(thunk: => R): R = {
    executeAndRelease(thunk, lock.lock().newParent)
  }
  def underLock[R](lockA: SubsumableLockEntryPoints, lockB: SubsumableLockEntryPoints)(thunk: => R): R = {
    assert(lockA.host == lockB.host)
    executeAndRelease(thunk, lockAndMerge(lockA, lockB).newParent)
  }
  private def executeAndRelease[R](thunk: => R, locked: SubsumableLock): R = {
    try { thunk } finally {
      locked.unlock()
    }
  }

  private def lockAndMerge(lockA: SubsumableLockEntryPoints, lockB: SubsumableLockEntryPoints): TryLockResult = {
    if(DEBUG) System.out.println(s"[${Thread.currentThread().getName}] syncing $lockA and $lockB")
//    lockAndMergeTryLockSpinOnly(lockA, lockB, new Backoff())
//    lockAndMergeWithBackoff(lockA, lockB, new Backoff())
//    lockAndMergeWithoutBackoff(lockA, lockB)
    lockAndMergeWithRemoteSpin(lockA, lockB, new Backoff())
  }

  @tailrec private def lockAndMergeTryLockSpinOnly(lockA: SubsumableLockEntryPoints, lockB: SubsumableLockEntryPoints, backoff: Backoff): TryLockResult = {
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
        lockAndMergeTryLockSpinOnly(lockB, lockA, backoff)
      }
    }
  }

  @tailrec private def lockAndMergeWithBackoff(lockA: SubsumableLockEntryPoints, lockB: SubsumableLockEntryPoints, backoff: Backoff): TryLockResult = {
    val resA = lockA.lock()
    val resB = lockB.trySubsume(resA)
    if(resB.isEmpty) {
      resA
    } else {
      resA.newParent.unlock()
      backoff.backoff()
      lockAndMergeWithBackoff(lockB, lockA, backoff)
    }
  }

  @tailrec private def lockAndMergeWithoutBackoff(lockA: SubsumableLockEntryPoints, lockB: SubsumableLockEntryPoints): TryLockResult = {
    val resA = lockA.lock()
    val resB = lockB.trySubsume(resA)
    if(resB.isEmpty) {
      resA
    } else {
      resA.newParent.unlock()
      lockAndMergeWithoutBackoff(lockB, lockA)
    }
  }

  private def lockAndMergeWithRemoteSpin(lockA: SubsumableLockEntryPoints, lockB: SubsumableLockEntryPoints, backoff: Backoff): TryLockResult = {
    val resA = lockA.lock()
    @tailrec def tryBandSpinAifFailed(resA: TryLockResult): TryLockResult = {
      val resB = lockB.trySubsume(resA)
      if (resB.isEmpty) {
        resA
      } else {
        val newResA = lockA.spinOnce(backoff.getAndIncrementBackoff())
        tryBandSpinAifFailed(newResA)
      }
    }
    tryBandSpinAifFailed(resA)
  }
}
