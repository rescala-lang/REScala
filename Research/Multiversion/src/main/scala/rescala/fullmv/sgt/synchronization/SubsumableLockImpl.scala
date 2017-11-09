package rescala.fullmv.sgt.synchronization

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.LockSupport

import rescala.fullmv.FullMVEngine
import rescala.fullmv.mirrors.{Host, SubsumableLockHost}
import rescala.fullmv.sgt.synchronization.SubsumableLock._
import rescala.parrp.Backoff

import scala.annotation.tailrec
import scala.concurrent.Future

sealed trait State
case object UnlockedRoot extends State
case object LockedRoot extends State
case class Subsumed(parent: SubsumableLock) extends State
case object GarbageCollected extends State
case class GarbageCollectedSubsumed(parent: SubsumableLock) extends State

class SubsumableLockImpl(override val host: SubsumableLockHost, override val guid: Host.GUID) extends SubsumableLock {
  val state = new AtomicReference[State](UnlockedRoot)

  override def getLockedRoot: Future[Option[Host.GUID]] = {
    state.get match {
      case UnlockedRoot => futureNone
      case LockedRoot => Future.successful(Some(guid))
      case Subsumed(parent) => parent.getLockedRoot
      case GarbageCollected => futureNone
      case GarbageCollectedSubsumed(parent) => parent.getLockedRoot
    }
  }

  @tailrec final override def trySubsume0(hopCount: Int, lastHopWasGCd: Boolean, lockedNewParent: SubsumableLock): Future[(Int, Option[SubsumableLock])] = {
    assert(lockedNewParent.host == host, s"trySubsume $this to $lockedNewParent is hosted on ${lockedNewParent.host} different from $host")
    if(lockedNewParent == this) {
      assert(lockedNewParent eq this, s"instance caching broken? $this came into contact with reflection of itLockedRoot on same host")
      assert(state.get == LockedRoot, s"passed in a TryLockResult indicates that $this was successfully locked, but it currently isn't!")
      val addHops = hopCount + (if(lastHopWasGCd) 1 else 0)
      if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this to itLockedRoot reentrant success; $addHops new refs")
      if(addHops > 0) localAddRefs(addHops)
      futureZeroNone
    } else {
      state.get match {
        case UnlockedRoot =>
          val success = state.compareAndSet(UnlockedRoot, Subsumed(lockedNewParent))
          if(success) {
            val addHops = 2 + hopCount + (if(lastHopWasGCd) 1 else 0)
            if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this succeeded; passing $addHops new refs")
            lockedNewParent.localAddRefs(addHops)
            Future.successful((0, None))
          } else {
            if (DEBUG) println(s"[${Thread.currentThread().getName}]: retrying contended trySubsume $this to $lockedNewParent")
            trySubsume0(hopCount, lastHopWasGCd, lockedNewParent)
          }
        case LockedRoot =>
          val addHops = hopCount + (if(lastHopWasGCd) 1 else 0)
          if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this to $lockedNewParent blocked; $addHops new refs")
          if(addHops > 0) localAddRefs(addHops)
          Future.successful((0, Some(this)))
        case subsumed@Subsumed(parent) =>
          parent.trySubsume0(hopCount + 1, lastHopWasGCd = false, lockedNewParent).map { res =>
            trySwap(subsumed, res._2.getOrElse(lockedNewParent), res)
          }(FullMVEngine.notWorthToMoveToTaskpool)
        case GarbageCollected =>
          throw new AssertionError("let's see if this can happen..")
        case GarbageCollectedSubsumed(parent) =>
          parent.trySubsume0(hopCount, lastHopWasGCd = true, lockedNewParent)
      }
    }
  }

  val waiters = new ConcurrentLinkedQueue[Thread]()
  @tailrec final override def lock0(hopCount: Int, lastHopWasGCd: Boolean): Future[(Int, SubsumableLock)] = {
    state.get match {
      case UnlockedRoot =>
        if(state.compareAndSet(UnlockedRoot, LockedRoot)) {
          val addHops = hopCount + (if(lastHopWasGCd) 1 else 0)
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: locked $this; $addHops new refs")
          if(addHops > 0) localAddRefs(addHops)
          Future.successful((0, this))
        } else {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: retrying contended lock attempt of $this")
          lock0(hopCount, lastHopWasGCd)
        }
      case LockedRoot =>
        val thread = Thread.currentThread()
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: waiting for lock on $this")
        waiters.add(thread)

        while(waiters.peek() != thread || state.get == LockedRoot) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: parking on $this")
          LockSupport.park(this)
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: unparked on $this")
        }

        waiters.remove()
        lock0(hopCount, lastHopWasGCd)
      case subsumed@Subsumed(parent) =>
        LockSupport.unpark(waiters.peek())
        parent.lock0(hopCount + 1, lastHopWasGCd = false).map { res =>
          trySwap(subsumed, res._2, res)
        }(FullMVEngine.notWorthToMoveToTaskpool)
      case GarbageCollected =>
        throw new AssertionError("let's see if this can happen..")
      case GarbageCollectedSubsumed(parent) =>
        LockSupport.unpark(waiters.peek())
        parent.lock0(hopCount, lastHopWasGCd = true)
    }
  }

  private def unlock0(): Unit = {
    if (!state.compareAndSet(LockedRoot, UnlockedRoot)) throw new AssertionError(s"$this unlock failed due to contention!?")
    val peeked = waiters.peek()
    if (DEBUG) println(s"[${Thread.currentThread().getName}]: release $this, unparking $peeked")
    LockSupport.unpark(peeked)
  }

  override def asyncUnlock0(): Unit = synchronized {
    state.get match {
      case UnlockedRoot =>
        throw new IllegalStateException(s"unlock on unlocked $this")
      case LockedRoot =>
        unlock0()
        Future.successful(this)
      case Subsumed(parent) =>
        throw new IllegalStateException(s"unlock on subsumed $this")
      case GarbageCollected => throw new AssertionError("lock is always held together with a thread reference, so this should be impossible")
      case GarbageCollectedSubsumed(_) => throw new AssertionError("lock is always held together with a thread reference, so this should be impossible")
    }
  }

  override def spinOnce0(backoff: Long): Future[(Int, SubsumableLock)] = {
    // this method may seem silly, but serves as an local redirect for preventing back-and-forth remote messages.
    state.get match {
      case UnlockedRoot =>
        throw new IllegalStateException(s"spinOnce on unlocked $this")
      case LockedRoot =>
        unlock0()
        Backoff.milliSleepNanoSpin(backoff)
        lock0(0, lastHopWasGCd = false)
      case Subsumed(parent) =>
        throw new IllegalStateException(s"spinOnce on subsumed $this")
      case GarbageCollected => throw new AssertionError("lock is always held together with a thread reference, so this should be impossible")
      case GarbageCollectedSubsumed(_) => throw new AssertionError("lock is always held together with a thread reference, so this should be impossible")
    }
  }

  def trySwap[T](from: Subsumed, to: SubsumableLock, res: (Int, T)): (Int, T) = {
    if(from.parent == to) {
      res
    } else if(state.compareAndSet(from, Subsumed(to))) {
      if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this parent cas $from to $to succeeded")
      from.parent.localSubRefs(1)
      res
    } else {
      if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this parent cas $from to $to failed due to contention")
      val (failedRefChanges, t) = res
      (failedRefChanges + 1, t)
    }
  }

  override def toString: String = {
    val refs = refCount.get()
    s"Lock($guid on $host, ${if(refs <= 0) "gc'd" else refs + " refs"}, ${state.get})"
  }

  override def remoteAsyncUnlock(): Unit = {
    asyncUnlock0()
  }
  override def remoteLock(): Future[SubsumableLock] = {
    lock0(0, lastHopWasGCd = false).map{ case (failedRefChanges, newRoot) =>
      if(failedRefChanges != 0) {
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning lock result to remote, correcting $failedRefChanges failed ref changes to $newRoot")
        newRoot.localSubRefs(failedRefChanges)
      }
      newRoot
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }
  override def remoteSpinOnce(backoff: Long): Future[SubsumableLock] = {
    spinOnce0(backoff).map{ case (failedRefChanges, newRoot) =>
      if(failedRefChanges != 0) {
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning lock result to remote, correcting $failedRefChanges failed ref changes to $newRoot")
        newRoot.localSubRefs(failedRefChanges)
      }
      newRoot
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }
  override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = {
    trySubsume0(0, lastHopWasGCd = false, lockedNewParent).map { case (failedRefChanges, res) =>
      if(failedRefChanges != 0) {
        val newRoot = res.getOrElse(lockedNewParent)
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning trySubsume result to remote, correcting $failedRefChanges failed ref changes to $newRoot")
        newRoot.localSubRefs(failedRefChanges)
      }
      res
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }

  @tailrec final override protected def dumped(): Unit = {
    state.get match {
      case UnlockedRoot =>
        if(state.compareAndSet(UnlockedRoot, GarbageCollected)) {
          if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this no refs remaining, deallocated")
        } else {
          dumped()
        }
      case LockedRoot =>
        throw new AssertionError(s"$this was garbage collected while locked")
      case x@Subsumed(parent) =>
        if(state.compareAndSet(x, GarbageCollectedSubsumed(x.parent))) {
          if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this no refs remaining, deallocating and dropping ref on parent $parent")
          parent.localSubRefs(1)
        } else {
          dumped()
        }
      case GarbageCollected => throw new AssertionError("deallocation should only occur once")
      case GarbageCollectedSubsumed(_) => throw new AssertionError("deallocation should only occur once")
    }
  }
}
