package rescala.fullmv.sgt.synchronization

import java.util.concurrent.atomic.AtomicInteger

import rescala.fullmv.FullMVTurn
import rescala.fullmv.mirrors._
import rescala.parrp.Backoff

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

sealed trait TrySubsumeResult0
case class Successful(failedRefChanges: Int) extends TrySubsumeResult0
sealed trait TryLockResult0
case class Locked(failedRefChanges: Int, lockedRoot: SubsumableLock) extends TryLockResult0

case object GarbageCollected extends TrySubsumeResult0 with TryLockResult0
case class Blocked(failedRefChanges: Int, newParent: SubsumableLock) extends TrySubsumeResult0 with TryLockResult0

sealed trait TrySubsumeResult
case object Successful extends TrySubsumeResult
case object Blocked extends TrySubsumeResult
case object Deallocated extends TrySubsumeResult

trait SubsumableLockEntryPoint {
  def getLockedRoot: Future[Option[Host.GUID]]
  def tryLock(): Future[Option[SubsumableLock]]
  def trySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult]
}

trait SubsumableLock extends SubsumableLockProxy with Hosted {
  val refCount = new AtomicInteger(1)

  override val host: SubsumableLockHost

  def getLockedRoot: Future[Option[Host.GUID]]
  def tryLock0(hopCount: Int): Future[TryLockResult0]
  def trySubsume0(hopCount: Int, lockedNewParent: SubsumableLock): Future[TrySubsumeResult0]
  def asyncUnlock0(): Unit

  def asyncUnlock(): Unit = {
    asyncUnlock0()
    if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this unlocked, dropping thread reference")
    localSubRefs(1)
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
    assert(newCount > 0, s"addition of $refs refs on $this resulted in non-positive reference count")
  }

  def localSubRefs(refs: Int): Unit = {
    assert(refs > 0)
    val remaining = refCount.addAndGet(-refs)
    assert(remaining >= 0, s"deallocation of $refs refs on $this resulted in negative reference count")
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
    if (DEBUG) System.out.println(s"[${Thread.currentThread().getName}] syncing on SCC of $contender")
    val bo = new Backoff()
    @tailrec def reTryLock(): SubsumableLock = {
      Await.result(contender.tryLock(), timeout) match {
        case Some(newParent) =>
          if (DEBUG) System.out.println(s"[${Thread.currentThread().getName}] now owns SCC of $contender")
          newParent
        case None =>
          bo.backoff()
          reTryLock()
      }
    }
    reTryLock()
  }

  def acquireLock[R](defender: FullMVTurn, contender: FullMVTurn, timeout: Duration): Option[SubsumableLock] = {
    assert(defender.host == contender.host)
    if (DEBUG) System.out.println(s"[${Thread.currentThread().getName}] syncing $defender and $contender into a common SCC")
    val bo = new Backoff()
    @tailrec def reTryLock(): Option[SubsumableLock] = {
      Await.result(contender.tryLock(), timeout) match {
        case Some(lockedRoot) =>
          Await.result(defender.trySubsume(lockedRoot), timeout) match {
            case Successful =>
              if (DEBUG) System.out.println(s"[${Thread.currentThread().getName}] now owns SCC of $defender and $contender")
              Some(lockedRoot)
            case Blocked =>
              lockedRoot.asyncUnlock()
              bo.backoff()
              reTryLock()
            case Deallocated =>
              if (DEBUG) System.out.println(s"[${Thread.currentThread().getName}] aborting sync due to deallocation contention")
              lockedRoot.asyncUnlock()
              None
          }
        case None =>
          bo.backoff()
          reTryLock()
      }
    }
    reTryLock()
  }

  val futureNone: Future[None.type] = Future.successful(None)
}
