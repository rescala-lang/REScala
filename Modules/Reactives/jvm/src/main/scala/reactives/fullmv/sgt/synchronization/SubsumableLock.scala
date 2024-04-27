package reactives.fullmv.sgt.synchronization

import reactives.fullmv.mirrors.*

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.concurrent.Future

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

sealed trait LockStateResult
sealed trait LockStateResult0
case class LockedState(guid: Host.GUID) extends LockStateResult with LockStateResult0
case object UnlockedState extends LockStateResult with LockStateResult0 {
  val futured = Future.successful(this)
}
case object CompletedState extends LockStateResult {
  val futured = Future.successful(this)
}
case object ConcurrentDeallocation extends LockStateResult0 {
  val futured = Future.successful(this)
}

trait SubsumableLockEntryPoint {
  def getLockedRoot: Future[LockStateResult]
  // result has one thread reference counted
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

trait SubsumableLock extends SubsumableLockProxy with Hosted[SubsumableLock] {
  val refCount = new AtomicInteger(1)

  override val host: SubsumableLockHost

  def getLockedRoot: Future[LockStateResult0]
  // result will have one thread reference to present a uniform interface for remote calls
  // (a result received from remote would immediately be deallocated if the thread receiving it doesn't hold a reference)
  def tryLock0(hopCount: Int): Future[TryLockResult0]
  final def tryLock0NoTail(hopCount: Int): Future[TryLockResult0] = tryLock0(hopCount)
  // result will have one thread reference to present a uniform interface for remote calls
  // (a result received from remote would immediately be deallocated if the thread receiving it doesn't hold a reference)
  // parameter does have a thread reference counted from being locked, but this must be retained to be released upon unlock.
  def trySubsume0(hopCount: Int, lockedNewParent: SubsumableLock): Future[TrySubsumeResult0]
  def trySubsume0NoTail(hopCount: Int, lockedNewParent: SubsumableLock): Future[TrySubsumeResult0] =
    trySubsume0(hopCount, lockedNewParent)
  def asyncUnlock0(): Unit

  def asyncUnlock(): Unit = {
    asyncUnlock0() // .onComplete {
//      case Success(()) =>
//        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this dropping temporary thread reference after unlock")
    localSubRefs(1)
//      case Failure(t) =>
//        new Exception("remote async unlock failed", t).printStackTrace()
//    }(FullMVUtil.notWorthToMoveToTaskpool)
  }

  @tailrec final def tryLocalAddRefs(refs: Int): Boolean = {
    val before = refCount.get()
    if (before == 0) {
      false
    } else if (refCount.compareAndSet(before, before + refs)) {
      true
    } else {
      tryLocalAddRefs(refs)
    }
  }

  @tailrec final def localAddRefs(refs: Int): Unit = {
    assert(refs > 0)
    val before = refCount.get()
    assert(before > 0, s"cannot add refs on gc'd $this")
    if (!refCount.compareAndSet(before, before + refs)) localAddRefs(refs)
  }

  def localSubRefs(refs: Int): Unit = {
    assert(refs > 0)
    val remaining = refCount.addAndGet(-refs)
    if (remaining == 0) {
      host.dropInstance(guid, this)
      dumped()
    } else {
      assert(remaining > 0, s"deallocation of $refs refs on $this resulted in negative reference count")
    }
  }

  override def asyncRemoteRefDropped(): Unit = {
    // if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this dropping remote reference")
    localSubRefs(1)
  }

  protected def dumped(): Unit
}
