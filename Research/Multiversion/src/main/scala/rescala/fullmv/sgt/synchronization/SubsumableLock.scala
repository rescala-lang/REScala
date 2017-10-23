package rescala.fullmv.sgt.synchronization

import java.util.concurrent.atomic.AtomicInteger

import rescala.fullmv.mirrors._
import rescala.fullmv.transmitter.ReactiveTransmittable
import rescala.parrp.Backoff

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

trait SubsumableLockEntryPoint {
  def getLockedRoot: Future[Option[Host.GUID]]
  def lock(): Future[SubsumableLock]
  def trySubsume(lockedNewParent: SubsumableLock): Future[Boolean]
}

trait SubsumableLock extends SubsumableLockProxy with Hosted {
  val refCount = new AtomicInteger(1)

  override val host: SubsumableLockHost

  def getLockedRoot: Future[Option[Host.GUID]]
  def lock0(hopCount: Int): Future[(Int, SubsumableLock)]
  def spinOnce0(backoff: Long): Future[(Int, SubsumableLock)]
  // if failed, returns Some(newParent) that should be used as new Parent
  // if successful, returns None; lockedNewParent.newParent should be used as newParent.
  // newParent is not a uniform part of all possible return values to avoid establishing unnecessary back-and-forth remote paths
  def trySubsume0(hopCount: Int, lockedNewParent: SubsumableLock): Future[(Int, Option[SubsumableLock])]
  def asyncUnlock0(): Unit

  def asyncUnlock(): Unit = {
    asyncUnlock0()
    if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this unlocked, dropping thread reference")
    localSubRefs(1)
  }
  def spinOnce(backoff: Long): Future[SubsumableLock] = {
    spinOnce0(backoff).map{ case (failedRefChanges, newRoot) =>
      if(newRoot == this) {
        assert(failedRefChanges == 0)
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this spun")
      } else {
        if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this was subsumed during spin, correcting $failedRefChanges failed ref changes to $newRoot before dropping thread reference")
        if (failedRefChanges != 0) newRoot.localSubRefs(failedRefChanges)
        localSubRefs(1)
      }
      newRoot
    }(ReactiveTransmittable.notWorthToMoveToTaskpool)
  }

  def localAddRefs(refs: Int): Unit = {
    assert(refs > 0)
    assert(refCount.getAndAdd(refs) > 0)
  }
  def localSubRefs(refs: Int): Unit = {
    assert(refs > 0)
    val remaining = refCount.addAndGet(-refs)
    assert(remaining >= 0)
    if(remaining == 0 && refCount.compareAndSet(0, Int.MinValue / 2)) {
      if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this no refs remaining, deallocating")
      host.dropInstance(guid, this)
      dumped()
    }
  }
  override def asyncRemoteRefDropped(): Unit = {
    if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this dropping remote reference")
    localSubRefs(1)
  }
  protected def dumped(): Unit
}

object SubsumableLock {
  val DEBUG = false
  def underLock[R](lockA: SubsumableLockEntryPoint with Hosted, lockB: SubsumableLockEntryPoint with Hosted, timeout: Duration)(thunk: => R): R = {
    assert(lockA.host == lockB.host)
    if(DEBUG) System.out.println(s"[${Thread.currentThread().getName}] syncing $lockA and $lockB")
    val lockedRootA = Await.result(lockA.lock(), timeout)

    val backoff = new Backoff()
    @inline @tailrec def trySecondAndSpinFirstIfFailed(lockedRootA: SubsumableLock): SubsumableLock = {
      if (Await.result(lockB.trySubsume(lockedRootA), timeout)) {
        lockedRootA
      } else {
        val newLockedRootA = Await.result(lockedRootA.spinOnce(backoff.getAndIncrementBackoff()), timeout)
        trySecondAndSpinFirstIfFailed(newLockedRootA)
      }
    }
    val commonLockedRoot = trySecondAndSpinFirstIfFailed(lockedRootA)

    try { thunk } finally {
      commonLockedRoot.asyncUnlock()
    }
  }

  val futureNone: Future[None.type] = Future.successful(None)
  val futureZeroNone: Future[(Int, None.type)] = Future.successful((0, None))
}
