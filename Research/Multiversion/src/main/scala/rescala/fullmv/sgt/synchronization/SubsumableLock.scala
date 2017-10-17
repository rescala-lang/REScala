package rescala.fullmv.sgt.synchronization

import java.util.concurrent.atomic.AtomicInteger

import rescala.fullmv.mirrors._
import rescala.parrp.Backoff

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

trait SubsumableLockEntryPoint {
  def getLockedRoot: Future[Option[Host.GUID]]
  def lock(): Future[SubsumableLock]
  def spinOnce(backoff: Long): Future[SubsumableLock]
  def trySubsume(lockedNewParent: SubsumableLock): Future[Boolean]
}

trait SubsumableLock extends SubsumableLockProxy with Hosted {
  val refCount = new AtomicInteger(1)

  override val host: SubsumableLockHost
  def getLockedRoot: Future[Option[Host.GUID]]
  def lock(hopCount: Int): Future[(Int, SubsumableLock)]
  def spinOnce(hopCount: Int, backoff: Long): Future[(Int, SubsumableLock)]
  // if failed, returns Some(newParent) that should be used as new Parent
  // if successful, returns None; lockedNewParent.newParent should be used as newParent.
  // newParent is not a uniform part of all possible return values to avoid establishing unnecessary back-and-forth remote paths
  def trySubsume(hopCount: Int, lockedNewParent: SubsumableLock): Future[(Int, Option[SubsumableLock])]
  def unlock(): Unit

  def addRefs(refs: Int): Future[Unit] = {
    assert(refCount.getAndAdd(refs) > 0)
    if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this $refs new refs.")
    Future.unit
  }
  def asyncSubRefs(refs: Int): Unit = {
    val remaining = refCount.addAndGet(refs)
    if(remaining == 0 && refCount.compareAndSet(0, Int.MinValue / 2)) {
      host.dropInstance(guid, this)
      if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this dumped $refs refs, deallocated")
    } else if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this dumped $refs refs, $remaining remaining")
  }
  override def remoteRefDropped(): Unit = asyncSubRefs(1)
}

object SubsumableLock {
  val DEBUG = false
  def underLock[R](lock: SubsumableLockProxy with Hosted, timeout: Duration)(thunk: => R): R = {
    executeAndRelease(thunk, Await.result(lock.lock(), timeout))
  }
  def underLock[R](lockA: SubsumableLockEntryPoint with Hosted, lockB: SubsumableLockEntryPoint with Hosted, timeout: Duration)(thunk: => R): R = {
    assert(lockA.host == lockB.host)
    if(DEBUG) System.out.println(s"[${Thread.currentThread().getName}] syncing $lockA and $lockB")
    val backoff = new Backoff()
    val lockedRoot = Await.result(lockA.lock(), timeout)
    @tailrec def trySecondAndSpinFirstIfFailed(lockedRoot: SubsumableLock): SubsumableLock = {
      if (Await.result(lockB.trySubsume(lockedRoot), timeout)) {
        lockedRoot
      } else {
        val newLockedRoot = Await.result(lockA.spinOnce(backoff.getAndIncrementBackoff()), timeout)
        trySecondAndSpinFirstIfFailed(newLockedRoot)
      }
    }
    executeAndRelease(thunk, trySecondAndSpinFirstIfFailed(lockedRoot))
  }

  private def executeAndRelease[R](thunk: => R, locked: SubsumableLock): R = {
    try { thunk } finally {
      locked.unlock()
    }
  }

  val futureNone: Future[None.type] = Future.successful(None)
  val futureZeroNone: Future[(Int, None.type)] = Future.successful((0, None))
}
