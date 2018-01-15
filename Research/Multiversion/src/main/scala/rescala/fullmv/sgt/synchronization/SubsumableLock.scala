package rescala.fullmv.sgt.synchronization

import java.util.concurrent.atomic.AtomicInteger

import rescala.fullmv.{FullMVEngine, FullMVTurn}
import rescala.fullmv.mirrors._
import rescala.parrp.Backoff

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration


sealed trait TrySubsumeResult
case object Successful extends TrySubsumeResult
case object Blocked extends TrySubsumeResult
case object Deallocated extends TrySubsumeResult

trait SubsumableLockEntryPoint {
  def getLockedRoot: Future[Option[Host.GUID]]
  def lock(): Future[SubsumableLock]
  def trySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult]
}

trait SubsumableLock extends SubsumableLockProxy with Hosted {
  val refCount = new AtomicInteger(1)

  override val host: SubsumableLockHost

  def getLockedRoot: Future[Option[Host.GUID]]
  def lock0(hopCount: Int, lastHopWasGCd: Boolean): Future[(Int, SubsumableLock)]
  def spinOnce0(backoff: Long): Future[(Int, SubsumableLock)]
  // if failed, returns Some(newParent) that should be used as new Parent
  // if successful, returns None; lockedNewParent.newParent should be used as newParent.
  // newParent is not a uniform part of all possible return values to avoid establishing unnecessary back-and-forth remote paths
  def trySubsume0(hopCount: Int, lastHopWasGCd: Boolean, lockedNewParent: SubsumableLock): Future[(Int, Option[SubsumableLock])]
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
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }

  def tryNewLocalRef(): Boolean = {
    val after = refCount.addAndGet(1)
    if(after > 0) {
      true
    } else {
      refCount.addAndGet(-1)
      false
    }
  }
  def localAddRefs(refs: Int): Unit = {
    assert(refs > 0)
    val newCount = refCount.getAndAdd(refs)
    assert(newCount > 0)
  }
  def localSubRefs(refs: Int): Unit = {
    assert(refs > 0)
    val remaining = refCount.addAndGet(-refs)
    assert(remaining >= 0)
    if(remaining == 0 && refCount.compareAndSet(0, Int.MinValue / 2)) {
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

  def acquireLock[R](contender: FullMVTurn, timeout: Duration): SubsumableLock = {
    if (DEBUG) System.out.println(s"[${Thread.currentThread().getName}] syncing $contender")
    Await.result(contender.lock(), timeout)
  }

  def acquireLock[R](defender: FullMVTurn, contender: FullMVTurn, timeout: Duration): Option[SubsumableLock] = {
    assert(defender.host == contender.host)
    if (DEBUG) System.out.println(s"[${Thread.currentThread().getName}] syncing $defender and $contender")
    trySubsumeDefender(acquireLock(contender, timeout), defender, timeout)
  }

  def trySubsumeDefender(lockedRootA: SubsumableLock, defender: FullMVTurn, timeout: Duration): Option[SubsumableLock] = {
    val backoff = new Backoff()
    @inline @tailrec def trySecondAndSpinFirstIfFailed(lockedRootA: SubsumableLock): Option[SubsumableLock] = {
      Await.result(defender.trySubsume(lockedRootA), timeout) match {
        case Successful =>
          Some(lockedRootA)
        case Blocked =>
          val newLockedRootA = Await.result(lockedRootA.spinOnce(backoff.getAndIncrementBackoff()), timeout)
          trySecondAndSpinFirstIfFailed(newLockedRootA)
        case Deallocated =>
          lockedRootA.asyncUnlock()
          None
      }
    }
    trySecondAndSpinFirstIfFailed(lockedRootA)
  }

  def underLock[R](contender: FullMVTurn, timeout: Duration)(thunk: => R): R = {
    val lockedRoot = acquireLock(contender, timeout)
    runThunkAndUnlock(lockedRoot, thunk)
  }

  private def runThunkAndUnlock[R](lockedRoot: SubsumableLock, thunk: => R) = {
    try {
      thunk
    } finally {
      lockedRoot.asyncUnlock()
    }
  }

  def underLock[R](defender: FullMVTurn, contender: FullMVTurn, timeout: Duration)(thunk: => R): Option[R] = {
    val res: Option[SubsumableLock] = acquireLock(defender, contender, timeout)
    res.map(runThunkAndUnlock(_, thunk))
  }

  val futureNone: Future[None.type] = Future.successful(None)
  val futureZeroNone: Future[(Int, None.type)] = Future.successful((0, None))
}
