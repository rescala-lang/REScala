package rescala.fullmv.sgt.synchronization

import java.util.concurrent.atomic.AtomicInteger

import rescala.fullmv.FullMVTurn
import rescala.fullmv.mirrors._
import rescala.parrp.Backoff

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

sealed trait TryLockResult
case class Locked(lock: SubsumableLock) extends TryLockResult
sealed trait TrySubsumeResult
case object Successful extends TrySubsumeResult {
  val futured = Future.successful(this)
}
case object Blocked extends TrySubsumeResult with TryLockResult {
  val futured = Future.successful(this)
}
case object Deallocated extends TrySubsumeResult with TryLockResult {
  val futured = Future.successful(this)
}

trait SubsumableLockEntryPoint {
  def getLockedRoot: Future[Option[Host.GUID]]
  def tryLock(): Future[TryLockResult]
  def trySubsume(lockedNewParent: SubsumableLock): Future[TrySubsumeResult]
}

sealed trait TrySubsumeResult0
case class Successful0(failedRefChanges: Int) extends TrySubsumeResult0
object Successful0 { val zeroFutured = Future.successful(Successful0(0)) }
sealed trait TryLockResult0
case class Locked0(failedRefChanges: Int, lockedRoot: SubsumableLock) extends TryLockResult0

case class Blocked0(failedRefChanges: Int, newParent: SubsumableLock) extends TrySubsumeResult0 with TryLockResult0
case object GarbageCollected0 extends TrySubsumeResult0 with TryLockResult0 {
  val futured = Future.successful(this)
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

  @tailrec final def tryNewLocalRef(): Boolean = {
    val before = refCount.get()
    if(before == 0) {
      false
    } else if(refCount.compareAndSet(before, before + 1)) {
      true
    } else {
      tryNewLocalRef()
    }
  }

  def localAddRefs(refs: Int): Unit = {
    assert(refs > 0)
    val previousCount = refCount.getAndAdd(refs)
    assert(previousCount > 0, s"cannot add refs on gc'd $this")
  }

  def localSubRefs(refs: Int): Unit = {
    assert(refs > 0)
    val remaining = refCount.addAndGet(-refs)
    if(remaining == 0) {
      host.dropInstance(guid, this)
      dumped()
    } else {
      assert(remaining > 0, s"deallocation of $refs refs on $this resulted in negative reference count")
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
        case Locked(newParent) =>
          if (DEBUG) System.out.println(s"[${Thread.currentThread().getName}] now owns SCC of $contender")
          newParent
        case Blocked =>
          bo.backoff()
          reTryLock()
        case Deallocated =>
          throw new AssertionError("this should be impossible we're calling tryLock on the contender only, which cannot deallocate concurrently.")
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
        case Locked(lockedRoot) =>
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
        case Blocked =>
          bo.backoff()
          reTryLock()
        case Deallocated =>
          throw new AssertionError("this should be impossible we're calling tryLock on the contender only, which cannot deallocate concurrently.")
      }
    }
    reTryLock()
  }

//  def tryLock[R](defender: FullMVTurn, contender: FullMVTurn, timeout: Duration): TryLockResult = {
//    assert(defender.host == contender.host)
//    if (DEBUG) System.out.println(s"[${Thread.currentThread().getName}] syncing $defender and $contender into a common SCC")
//    Await.result(contender.tryLock(), timeout) match {
//      case Locked(lockedRoot) =>
//        Await.result(defender.trySubsume(lockedRoot), timeout) match {
//          case Successful =>
//            if (DEBUG) System.out.println(s"[${Thread.currentThread().getName}] now owns SCC of $defender and $contender")
//            Locked(lockedRoot)
//          case Blocked =>
//            lockedRoot.asyncUnlock()
//            Blocked
//          case Deallocated =>
//            if (DEBUG) System.out.println(s"[${Thread.currentThread().getName}] aborting sync due to deallocation contention")
//            lockedRoot.asyncUnlock()
//            Deallocated
//        }
//      case Blocked =>
//        Blocked
//      case Deallocated =>
//        throw new AssertionError("this should be impossible we're calling tryLock on the contender only, which cannot deallocate concurrently.")
//    }
//  }

  val futureNone: Future[None.type] = Future.successful(None)
}
