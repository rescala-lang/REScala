package rescala.fullmv.sgt.synchronization

import java.util.concurrent.atomic.AtomicReference

import rescala.fullmv.FullMVEngine
import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.SubsumableLock._

import scala.annotation.tailrec
import scala.concurrent.Future

class SubsumableLockImpl(override val host: SubsumableLockHost, override val guid: Host.GUID) extends SubsumableLock {
  Self =>
  val state = new AtomicReference[SubsumableLock](null)

  override def getLockedRoot: Future[Option[Host.GUID]] = {
    state.get match {
      case null => futureNone
      case Self => Future.successful(Some(guid))
      case host.dummy => futureNone
      case parent => parent.getLockedRoot
    }
  }

  @tailrec final override def trySubsume0(hopCount: Int, lockedNewParent: SubsumableLock): Future[TrySubsumeResult0] = {
    assert(lockedNewParent.host == host, s"trySubsume $this to $lockedNewParent is hosted on ${lockedNewParent.host} different from $host")
    if(lockedNewParent == this) {
      assert(lockedNewParent eq this, s"instance caching broken? $this came into contact with reflection of itself on same host")
      assert(state.get == Self, s"passed in a TryLockResult indicates that $this was successfully locked, but it currently isn't!")
      if (DEBUG) println(s"[${Thread.currentThread().getName}] trySubsume $this to itself reentrant success; $hopCount new refs")
      if(hopCount > 0) localAddRefs(hopCount)
      Future.successful(Successful(0))
    } else {
      state.get match {
        case null =>
          val success = state.compareAndSet(null, lockedNewParent)
          if(success) {
            if (DEBUG) println(s"[${Thread.currentThread().getName}] trySubsume $this succeeded; passing ${hopCount + 2} new refs")
            lockedNewParent.localAddRefs(hopCount + 2)
            Future.successful(Successful(0))
          } else {
            if (DEBUG) println(s"[${Thread.currentThread().getName}] retrying contended trySubsume $this to $lockedNewParent")
            trySubsume0(hopCount, lockedNewParent)
          }
        case Self =>
          if (DEBUG) println(s"[${Thread.currentThread().getName}] trySubsume $this to $lockedNewParent blocked; $hopCount new refs")
          if(hopCount > 0) localAddRefs(hopCount)
          Future.successful(Blocked(0, this))
        case host.dummy =>
          Future.successful(GarbageCollected)
        case parent =>
          parent.trySubsume0(hopCount + 1, lockedNewParent).flatMap {
            case Successful(failedRefChanges) =>
              Future.successful(Successful(failedRefChanges + trySwap(parent, lockedNewParent)))
            case Blocked(failedRefChanges, newParent) =>
              Future.successful(Blocked(failedRefChanges + trySwap(parent, newParent), newParent))
            case GarbageCollected =>
              this.asInstanceOf[SubsumableLock].trySubsume0(hopCount, lockedNewParent)
          }(FullMVEngine.notWorthToMoveToTaskpool)
      }
    }
  }

  @tailrec final override def tryLock0(hopCount: Int): Future[TryLockResult0] = {
    state.get match {
      case null =>
        if(state.compareAndSet(null, Self)) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}] tryLocked $this; ${hopCount + 1} new refs")
          localAddRefs(hopCount + 1)
          Future.successful(Locked(0, this))
        } else {
          if(DEBUG) println(s"[${Thread.currentThread().getName}] retrying contended tryLock attempt of $this")
          tryLock0(hopCount)
        }
      case Self =>
        if(DEBUG) println(s"[${Thread.currentThread().getName}] tryLock $this blocked; $hopCount new refs")
        if(hopCount > 0) localAddRefs(hopCount)
        Future.successful(Blocked(0, this))
      case host.dummy =>
        Future.successful(GarbageCollected)
      case parent =>
        parent.tryLock0(hopCount + 1).flatMap {
          case Locked(failedRefChanges, newRoot) =>
            Future.successful(Locked(failedRefChanges + trySwap(parent, newRoot), newRoot))
          case Blocked(failedRefChanges, newRoot) =>
            Future.successful(Blocked(failedRefChanges + trySwap(parent, newRoot), newRoot))
          case GarbageCollected =>
            if(DEBUG) println(s"[${Thread.currentThread().getName}] retrying tryLock $this after parent gc")
            this.asInstanceOf[SubsumableLock].tryLock0(hopCount)
        }(FullMVEngine.notWorthToMoveToTaskpool)
    }
  }

  override def asyncUnlock0(): Unit = synchronized {
    state.get match {
      case null => throw new IllegalStateException(s"unlock on unlocked $this")
      case Self => if (!state.compareAndSet(Self, null)) throw new AssertionError(s"$this unlock failed due to contention!?")
      case host.dummy => throw new AssertionError("lock is always held together with a thread reference, so this should be impossible")
      case parent => throw new IllegalStateException(s"unlock on subsumed $this")
    }
  }

  private def trySwap[T](from: SubsumableLock, to: SubsumableLock): Int = {
    if(from == to) {
      0
    } else if(state.compareAndSet(from, to)) {
      if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this parent cas $from to $to succeeded")
      from.localSubRefs(1)
      0
    } else {
      if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this parent cas $from to $to failed due to contention")
      1
    }
  }

  override def toString: String = {
    val refs = refCount.get()
    s"Lock($guid on $host, ${if(refs <= 0) "gc'd" else refs + " refs"}, ${state.get match {
      case null => "unlocked"
      case Self => "locked"
      case host.dummy => "gc'd"
      case parent => "subsumed: " + parent
    }})"
  }

  override def remoteAsyncUnlock(): Unit = {
    asyncUnlock0()
  }
  override def remoteTryLock(): Future[RemoteTryLockResult] = {
    tryLock0(0).map{
      case Locked(failedRefChanges, newRoot) =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning tryLock success to remote, correcting $failedRefChanges failed ref changes to $newRoot")
        if(failedRefChanges != 0) newRoot.localSubRefs(failedRefChanges)
        RemoteLocked(newRoot)
      case Blocked(failedRefChanges, newRoot) =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning tryLock failure to remote, correcting $failedRefChanges failed ref changes to $newRoot")
        if(failedRefChanges != 0) newRoot.localSubRefs(failedRefChanges)
        RemoteBlocked(newRoot)
      case GarbageCollected =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning tryLock abort to remote")
        RemoteGCd
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }

  override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[RemoteTrySubsumeResult] = {
    trySubsume0(0, lockedNewParent).map {
      case Successful(failedRefChanges) =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning trySubsume success to remote, correcting $failedRefChanges failed ref changes to $lockedNewParent")
        if(failedRefChanges != 0) lockedNewParent.localSubRefs(failedRefChanges)
        RemoteSubsumed
      case Blocked(failedRefChanges, newRoot) =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning trySubsume failure to remote, correcting $failedRefChanges failed ref changes to $newRoot")
        if(failedRefChanges != 0) newRoot.localSubRefs(failedRefChanges)
        RemoteBlocked(newRoot)
      case GarbageCollected =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning trySubsume abort to remote")
        RemoteGCd
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }

  override protected def dumped(): Unit = {
    state.getAndSet(host.dummy) match {
      case null => // ok
      case Self => throw new AssertionError(s"$this was garbage collected while locked")
      case host.dummy => throw new AssertionError(s"$this was already garbage collected earlier")
      case parent => // ok
    }
  }
}
