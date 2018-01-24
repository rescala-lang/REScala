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

  @tailrec final override def tryLock0(hopCount: Int): Future[TryLockResult0] = {
    state.get match {
      case null =>
        if(state.compareAndSet(null, Self)) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}] tryLocked $this; $hopCount new refs")
          // should be safe because we are now calling tryLock only on the contender, which means there is a guaranteed
          // reference that will not be deallocated concurrently?
          if(hopCount > 0) localAddRefs(hopCount)
          Future.successful(Locked0(0, this))
        } else {
          if(DEBUG) println(s"[${Thread.currentThread().getName}] retrying contended tryLock attempt of $this")
          tryLock0(hopCount)
        }
      case Self =>
        if(DEBUG) println(s"[${Thread.currentThread().getName}] tryLock $this blocked; $hopCount new refs")
        // not safe because held by different thread which could concurrently unlock and deallocate?
        if(hopCount == 0 || tryLocalAddRefs(hopCount)) {
          Future.successful(Blocked0(0, this))
        } else {
          GarbageCollected0.futured
        }
      case host.dummy =>
        GarbageCollected0.futured
      case parent =>
        parent.tryLock0(hopCount + 1).flatMap {
          case Locked0(failedRefChanges, newRoot) =>
            Future.successful(Locked0(failedRefChanges + trySwap(parent, newRoot), newRoot))
          case Blocked0(failedRefChanges, newRoot) =>
            Future.successful(Blocked0(failedRefChanges + trySwap(parent, newRoot), newRoot))
          case GarbageCollected0 =>
            if (DEBUG) println(s"[${Thread.currentThread().getName}] retrying tryLock $this after parent was concurrently deallocated")
            this.asInstanceOf[SubsumableLock].tryLock0(hopCount)
        }(FullMVEngine.notWorthToMoveToTaskpool)
    }
  }

  @tailrec final override def trySubsume0(hopCount: Int, lockedNewParent: SubsumableLock): Future[TrySubsumeResult0] = {
    assert(lockedNewParent.host == host, s"trySubsume $this to $lockedNewParent is hosted on ${lockedNewParent.host} different from $host")
    if(lockedNewParent == this) {
      assert(lockedNewParent eq this, s"instance caching broken? $this came into contact with reflection of itself on same host")
      assert(state.get == Self, s"passed in a TryLockResult indicates that $this was successfully locked, but it currently isn't!")
      if (DEBUG) println(s"[${Thread.currentThread().getName}] trySubsume $this to itself reentrant success; $hopCount new refs")
      // safe because locked by the current thread and thus cannot be deallocated
      if(hopCount > 0) localAddRefs(hopCount)
      Successful0.zeroFutured
    } else {
      state.get match {
        case null =>
          val success = state.compareAndSet(null, lockedNewParent)
          if(success) {
            if (DEBUG) println(s"[${Thread.currentThread().getName}] trySubsume $this succeeded; passing ${hopCount + 2} new refs")
            // safe because locked by the current thread and thus cannot be deallocated
            lockedNewParent.localAddRefs(hopCount + 2)
            Successful0.zeroFutured
          } else {
            if (DEBUG) println(s"[${Thread.currentThread().getName}] retrying contended trySubsume $this to $lockedNewParent")
            trySubsume0(hopCount, lockedNewParent)
          }
        case Self =>
          if (DEBUG) println(s"[${Thread.currentThread().getName}] trySubsume $this to $lockedNewParent blocked; $hopCount new refs")
          // not safe because held by different thread which could concurrently unlock and deallocate?
          if(hopCount == 0 || tryLocalAddRefs(hopCount)) {
            Future.successful(Blocked0(0, this))
          } else {
            GarbageCollected0.futured
          }
        case host.dummy =>
          GarbageCollected0.futured
        case parent =>
          parent.trySubsume0(hopCount + 1, lockedNewParent).flatMap {
            case Successful0(failedRefChanges) =>
              Future.successful(Successful0(failedRefChanges + trySwap(parent, lockedNewParent)))
            case Blocked0(failedRefChanges, newParent) =>
              Future.successful(Blocked0(failedRefChanges + trySwap(parent, newParent), newParent))
            case GarbageCollected0 =>
              if (DEBUG) println(s"[${Thread.currentThread().getName}] retrying trySubsume $this after parent was concurrently deallocated")
              this.asInstanceOf[SubsumableLock].trySubsume0(hopCount, lockedNewParent)
          }(FullMVEngine.notWorthToMoveToTaskpool)
      }
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
    s"Lock($guid on $host, ${if(refs <= 0) "gc'd" else refs + " refs"}, ${if (this eq host.dummy) "dummy" else state.get match {
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
    tryLock0(1).map{
      case Locked0(failedRefChanges, newRoot) =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning tryLock success to remote, correcting $failedRefChanges failed ref changes to $newRoot")
        if(failedRefChanges != 0) newRoot.localSubRefs(failedRefChanges)
        RemoteLocked(newRoot)
      case Blocked0(failedRefChanges, newRoot) =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning tryLock failure to remote, correcting $failedRefChanges failed ref changes to $newRoot")
        if(failedRefChanges != 0) newRoot.localSubRefs(failedRefChanges)
        RemoteBlocked(newRoot)
      case GarbageCollected0 =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning tryLock abort to remote")
        RemoteGCd
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }

  override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[RemoteTrySubsumeResult] = {
    trySubsume0(1, lockedNewParent).map {
      case Successful0(failedRefChanges) =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning trySubsume success to remote, correcting $failedRefChanges failed ref changes to $lockedNewParent (includes temporary remote parameter reference)")
        lockedNewParent.localSubRefs(failedRefChanges + 1)
        RemoteSubsumed
      case Blocked0(failedRefChanges, newRoot) =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning trySubsume failure to remote, correcting $failedRefChanges failed ref changes to $newRoot and dropping temporary remote parameter reference on $lockedNewParent")
        if(failedRefChanges != 0) newRoot.localSubRefs(failedRefChanges)
        lockedNewParent.localSubRefs(1)
        RemoteBlocked(newRoot)
      case GarbageCollected0 =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning trySubsume abort to remote")
        RemoteGCd
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }

  override protected def dumped(): Unit = {
    state.getAndSet(host.dummy) match {
      case null => if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this deallocated without parent")
      case Self => throw new AssertionError(s"$this was garbage collected while locked")
      case host.dummy => throw new AssertionError(s"$this was already garbage collected earlier")
      case parent =>
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this deallocated, dropping parent ref on $parent")
        parent.localSubRefs(1)
    }
  }
}
