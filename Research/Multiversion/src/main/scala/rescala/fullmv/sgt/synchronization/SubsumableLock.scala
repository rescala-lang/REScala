package rescala.fullmv.sgt.synchronization

import rescala.fullmv.mirrors._
import rescala.parrp.Backoff

import scala.annotation.tailrec

trait SubsumableLock extends SubsumableLockProxy with Hosted {
  override val host: SubsumableLockHost
}

object SubsumableLock {
  val DEBUG = false
  case class TryLockResult(success: Boolean, newParent: SubsumableLock)

  def underLock[R](lock: SubsumableLockProxy with Hosted)(thunk: => R): R = {
    executeAndRelease(thunk, lock.lock())
  }
  def underLock[R](lockA: SubsumableLockProxy with Hosted, lockB: SubsumableLockProxy with Hosted)(thunk: => R): R = {
    assert(lockA.host == lockB.host)
    executeAndRelease(thunk, lockAndMerge(lockA, lockB))
  }
  private def executeAndRelease[R](thunk: => R, locked: SubsumableLockProxy): R = {
    try { thunk } finally {
      locked.unlock()
    }
  }

  private def lockAndMerge(lockA: SubsumableLockProxy with Hosted, lockB: SubsumableLockProxy with Hosted): SubsumableLock = {
    if(DEBUG) System.out.println(s"[${Thread.currentThread().getName}] syncing $lockA and $lockB")
//    lockAndMergeTryLockSpinOnly(lockA, lockB, new Backoff())
//    lockAndMergeWithBackoff(lockA, lockB, new Backoff())
//    lockAndMergeWithoutBackoff(lockA, lockB)
    lockAndMergeWithRemoteSpin(lockA, lockB, new Backoff())
  }

  @tailrec private def lockAndMergeTryLockSpinOnly(lockA: SubsumableLockProxy with Hosted, lockB: SubsumableLockProxy with Hosted, backoff: Backoff): SubsumableLock = {
    val TryLockResult(success, lockedRoot) = lockA.tryLock()
    if(!success) {
      backoff.backoff()
      lockAndMergeTryLockSpinOnly(lockedRoot, lockB, backoff)
    } else {
      val resB = lockB.trySubsume(lockedRoot)
      if(resB.isEmpty) {
        lockedRoot
      } else {
        lockedRoot.unlock()
//        backoff.reset()
        backoff.backoff()
        lockAndMergeTryLockSpinOnly(lockB, lockA, backoff)
      }
    }
  }

  @tailrec private def lockAndMergeWithBackoff(lockA: SubsumableLockProxy with Hosted, lockB: SubsumableLockProxy with Hosted, backoff: Backoff): SubsumableLock = {
    val lockedRoot = lockA.lock()
    val resB = lockB.trySubsume(lockedRoot)
    if(resB.isEmpty) {
      lockedRoot
    } else {
      lockedRoot.unlock()
      backoff.backoff()
      lockAndMergeWithBackoff(lockB, lockA, backoff)
    }
  }

  @tailrec private def lockAndMergeWithoutBackoff(lockA: SubsumableLockProxy with Hosted, lockB: SubsumableLockProxy with Hosted): SubsumableLock = {
    val lockedRoot = lockA.lock()
    val resB = lockB.trySubsume(lockedRoot)
    if(resB.isEmpty) {
      lockedRoot
    } else {
      lockedRoot.unlock()
      lockAndMergeWithoutBackoff(lockB, lockA)
    }
  }

  private def lockAndMergeWithRemoteSpin(lockA: SubsumableLockProxy with Hosted, lockB: SubsumableLockProxy with Hosted, backoff: Backoff): SubsumableLock = {
    val lockedRoot = lockA.lock()
    @tailrec def tryBandSpinAifFailed(lockedRoot: SubsumableLock): SubsumableLock = {
      val resB = lockB.trySubsume(lockedRoot)
      if (resB.isEmpty) {
        lockedRoot
      } else {
        val newLockedRoot = lockA.spinOnce(backoff.getAndIncrementBackoff())
        tryBandSpinAifFailed(newLockedRoot)
      }
    }
    tryBandSpinAifFailed(lockedRoot)
  }
}
