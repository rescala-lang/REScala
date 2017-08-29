package rescala.fullmv.sgt.synchronization

import rescala.fullmv.mirrors._
import rescala.parrp.Backoff

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

trait SubsumableLock extends SubsumableLockProxy with Hosted {
  override val host: SubsumableLockHost
}

object SubsumableLock {
  val DEBUG = false
  case class TryLockResult(success: Boolean, newParent: SubsumableLock)

  def underLock[R](lock: SubsumableLockProxy with Hosted, timeout: Duration)(thunk: => R): R = {
    executeAndRelease(thunk, Await.result(lock.lock(), timeout), timeout)
  }
  def underLock[R](lockA: SubsumableLockProxy with Hosted, lockB: SubsumableLockProxy with Hosted, timeout: Duration)(thunk: => R): R = {
    assert(lockA.host == lockB.host)
    executeAndRelease(thunk, lockAndMerge(lockA, lockB, timeout), timeout)
  }
  private def executeAndRelease[R](thunk: => R, locked: SubsumableLockProxy, timeout: Duration): R = {
    try { thunk } finally {
      Await.result(locked.unlock(), timeout)
    }
  }

  private def lockAndMerge(lockA: SubsumableLockProxy with Hosted, lockB: SubsumableLockProxy with Hosted, timeout: Duration): SubsumableLock = {
    if(DEBUG) System.out.println(s"[${Thread.currentThread().getName}] syncing $lockA and $lockB")
//    lockAndMergeTryLockSpinOnly(lockA, lockB, timeout, new Backoff())
//    lockAndMergeWithBackoff(lockA, lockB, timeout, new Backoff())
//    lockAndMergeWithoutBackoff(lockA, lockB, timeout)
    lockAndMergeWithRemoteSpin(lockA, lockB, timeout, new Backoff())
  }

  @tailrec private def lockAndMergeTryLockSpinOnly(lockA: SubsumableLockProxy with Hosted, lockB: SubsumableLockProxy with Hosted, timeout: Duration, backoff: Backoff): SubsumableLock = {
    val TryLockResult(success, lockedRoot) = Await.result(lockA.tryLock(), timeout)
    if(!success) {
      backoff.backoff()
      lockAndMergeTryLockSpinOnly(lockedRoot, lockB, timeout, backoff)
    } else {
      val resB = Await.result(lockB.trySubsume(lockedRoot), timeout)
      if(resB.isEmpty) {
        lockedRoot
      } else {
        Await.result(lockedRoot.unlock(), timeout)
//        backoff.reset()
        backoff.backoff()
        lockAndMergeTryLockSpinOnly(lockB, lockA, timeout, backoff)
      }
    }
  }

  @tailrec private def lockAndMergeWithBackoff(lockA: SubsumableLockProxy with Hosted, lockB: SubsumableLockProxy with Hosted, timeout: Duration, backoff: Backoff): SubsumableLock = {
    val lockedRoot = Await.result(lockA.lock(), timeout)
    val resB = Await.result(lockB.trySubsume(lockedRoot), timeout)
    if(resB.isEmpty) {
      lockedRoot
    } else {
      Await.result(lockedRoot.unlock(), timeout)
      backoff.backoff()
      lockAndMergeWithBackoff(lockB, lockA, timeout, backoff)
    }
  }

  @tailrec private def lockAndMergeWithoutBackoff(lockA: SubsumableLockProxy with Hosted, lockB: SubsumableLockProxy with Hosted, timeout: Duration): SubsumableLock = {
    val lockedRoot = Await.result(lockA.lock(), timeout)
    val resB = Await.result(lockB.trySubsume(lockedRoot), timeout)
    if(resB.isEmpty) {
      lockedRoot
    } else {
      Await.result(lockedRoot.unlock(), timeout)
      lockAndMergeWithoutBackoff(lockB, lockA, timeout)
    }
  }

  private def lockAndMergeWithRemoteSpin(lockA: SubsumableLockProxy with Hosted, lockB: SubsumableLockProxy with Hosted, timeout: Duration, backoff: Backoff): SubsumableLock = {
    val lockedRoot = Await.result(lockA.lock(), timeout)
    @tailrec def tryBandSpinAifFailed(lockedRoot: SubsumableLock): SubsumableLock = {
      val resB = Await.result(lockB.trySubsume(lockedRoot), timeout)
      if (resB.isEmpty) {
        lockedRoot
      } else {
        val newLockedRoot = Await.result(lockA.spinOnce(backoff.getAndIncrementBackoff()), timeout)
        tryBandSpinAifFailed(newLockedRoot)
      }
    }
    tryBandSpinAifFailed(lockedRoot)
  }

  val futureNone: Future[None.type] = Future.successful(None)
}
