package rescala.fullmv.sgt.synchronization

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
  sealed trait RootWithLockStatus {
    val root: SubsumableLock
  }
  case class Locked(root: SubsumableLock, key: Any) extends RootWithLockStatus
  case class Unlocked(root: SubsumableLock) extends RootWithLockStatus

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
  private val initialBackoff: Long = 100L * 1000L
  private val maxBackoff: Long = 10L * 1000L * 1000L
  private val factor: Double = 1.2D
  private def lockAndMerge(key: Any, lockA: SubsumableLock, lockB: SubsumableLock): SubsumableLock = {
    lockAndMerge0(key, lockA, lockB, initialBackoff)
  }
  @tailrec private def lockAndMerge0(key: Any, lockA: SubsumableLock, lockB: SubsumableLock, backoff: Long): SubsumableLock = {
    val lockedRootA = lockA.lock(key)
    val resB = lockB.tryLock(key)
    if(resB.newRoot == lockedRootA) {
      assert(resB.success)
      lockedRootA
    } else if(resB.success) {
      lockedRootA.subsume(resB.newRoot)
    } else {
      lockedRootA.unlock(key)
      if (backoff < 1000000L) {
        val done = System.nanoTime() + backoff
        while (System.nanoTime() < done) {Thread.`yield`()}
      } else {
        Thread.sleep(backoff / 1000000L)
      }
      lockAndMerge0(key, resB.newRoot, lockedRootA, Math.min(Math.round(backoff * factor), maxBackoff))
    }
  }
}
