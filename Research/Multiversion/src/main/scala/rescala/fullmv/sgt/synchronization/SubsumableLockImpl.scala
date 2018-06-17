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

  override def getLockedRoot: Future[LockStateResult0] = {
    state.get match {
      case null => UnlockedState.futured
      case Self => Future.successful(LockedState(guid))
      case host.dummy => ConcurrentDeallocation.futured
      case parent => parent.getLockedRoot
    }
  }

  @tailrec final override def tryLock0(hopCount: Int): Future[TryLockResult0] = {
    state.get match {
      case null =>
        if(state.compareAndSet(null, Self)) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}] tryLocked $this; ${hopCount + 1} new refs (includes new thread reference)")
          // should be safe because we are now calling tryLock only on the contender, which means there is a guaranteed
          // reference that will not be deallocated concurrently?
          localAddRefs(hopCount + 1)
          Future.successful(Locked0(0, this))
        } else {
          if(DEBUG) println(s"[${Thread.currentThread().getName}] retrying contended tryLock attempt of $this")
          tryLock0(hopCount)
        }
      case Self =>
        // not safe because held by different thread which could concurrently unlock and deallocate?
        if(tryLocalAddRefs(hopCount + 1)) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}] tryLock $this blocked; added ${hopCount + 1} new refs (includes new thread reference)")
          Future.successful(Blocked0(0, this))
        } else {
          if (DEBUG) println(s"[${Thread.currentThread().getName}] tryLock $this blocked, but no refs added due to concurrent release+gc")
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
      if (DEBUG) println(s"[${Thread.currentThread().getName}] trySubsume $this to itself reentrant success; $hopCount new refs (no thread reference)")
      // safe because locked by the current thread and thus cannot be deallocated
      assert(hopCount >= 0, s"this case should be caught by the assertion in remoteTrySubsume already")
      if(hopCount > 0) localAddRefs(hopCount)
      Successful0.zeroFutured
    } else {
      state.get match {
        case null =>
          // safe because locked by the current thread and thus cannot be deallocated
          // must count the references before actually establishing them, though, as
          // their removal might otherwise temporarily under-drop the counter and
          // thereby deallocate the lock prematurely
          lockedNewParent.localAddRefs(hopCount + 2)
          val success = state.compareAndSet(null, lockedNewParent)
          if(success) {
            if (DEBUG) println(s"[${Thread.currentThread().getName}] trySubsume $this succeeded; added ${hopCount + 2} new refs (new subsumption reference, no thread reference)")
            Successful0.zeroFutured
          } else {
            lockedNewParent.localSubRefs(hopCount + 2)
            if (DEBUG) println(s"[${Thread.currentThread().getName}] retrying contended trySubsume $this to $lockedNewParent (re-removed ${hopCount + 2} prematurely added references)")
            trySubsume0(hopCount, lockedNewParent)
          }
        case Self =>
          // not safe because held by different thread which could concurrently unlock and deallocate?
          if(tryLocalAddRefs(hopCount + 1)) {
            if (DEBUG) println(s"[${Thread.currentThread().getName}] trySubsume $this to $lockedNewParent blocked; added ${hopCount + 1} new refs (includes new thread reference)")
            Future.successful(Blocked0(0, this))
          } else {
            if (DEBUG) println(s"[${Thread.currentThread().getName}] trySubsume $this to $lockedNewParent blocked, but no refs added due to concurrent release+gc")
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
      case Self =>
        if (!state.compareAndSet(Self, null)){
          throw new AssertionError(s"$this unlock failed due to contention!?")
        } else {
          if (DEBUG) println(s"[${Thread.currentThread().getName}] $this unlocked")
        }
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

  override def remoteUnlock(): Unit = {
    asyncUnlock0()
  }

  @tailrec final override def remoteTryLock(): Future[RemoteTryLockResult] = {
    if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this dispatching remote tryLock request locally")
    state.get match {
      case null =>
        if(state.compareAndSet(null, Self)) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}] remote tryLocked $this; adding remote connection establishment reference")
          // should be safe because we are now calling tryLock only on the contender, which means there is a guaranteed
          // reference that will not be deallocated concurrently?
          localAddRefs(1)
          Future.successful(RemoteLocked(this))
        } else {
          if(DEBUG) println(s"[${Thread.currentThread().getName}] retrying contended remote tryLock attempt of $this")
          remoteTryLock()
        }
      case Self =>
        // not safe because held by different thread which could concurrently unlock and deallocate?
        if(tryLocalAddRefs(1)) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}] remote tryLock $this blocked; added new remote connection establishment reference")
          Future.successful(RemoteBlocked(this))
        } else {
          if (DEBUG) println(s"[${Thread.currentThread().getName}] remote tryLock $this blocked, but no refs added due to concurrent release+gc")
          RemoteGCd.futured
        }
      case host.dummy =>
        if (DEBUG) println(s"[${Thread.currentThread().getName}] remote tryLock $this failed to deallocation race")
        RemoteGCd.futured
      case parent =>
        parent.tryLock0(0).flatMap{
          case Locked0(failedRefChanges, newRoot) =>
            val finalFailedRefChanges = failedRefChanges + trySwap(parent, newRoot)
            if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning tryLock success to remote, correcting $finalFailedRefChanges failed ref changes to $newRoot (thread reference is retained for remote connection establishment)")
            if(finalFailedRefChanges != 0) newRoot.localSubRefs(finalFailedRefChanges)
            Future.successful(RemoteLocked(newRoot))
          case Blocked0(failedRefChanges, newRoot) =>
            val finalFailedRefChanges = failedRefChanges + trySwap(parent, newRoot)
            if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning tryLock blocked under $newRoot to remote, correcting $finalFailedRefChanges failed ref changes (thread reference is retained for remote connection establishment)")
            if(finalFailedRefChanges != 0) newRoot.localSubRefs(finalFailedRefChanges)
            Future.successful(RemoteBlocked(newRoot))
          case GarbageCollected0 =>
            if (DEBUG) println(s"[${Thread.currentThread().getName}] retrying remote tryLock $this after parent was concurrently deallocated")
            this.asInstanceOf[SubsumableLock].remoteTryLock()
        }(FullMVEngine.notWorthToMoveToTaskpool)
    }
  }

  @tailrec final override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[RemoteTrySubsumeResult] = {
    assert(lockedNewParent != this, s"reflection should have handled this as reentrant subsume")
    if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this dispatching remote trySubsume $lockedNewParent request locally")
    state.get match {
      case null =>
        val success = state.compareAndSet(null, lockedNewParent)
        if(success) {
          if (DEBUG) println(s"[${Thread.currentThread().getName}] remote trySubsume $this succeeded; retaining temporary remote parameter reference as new subsumption reference")
          // safe because locked by the current thread and thus cannot be deallocated
//          lockedNewParent.localAddRefs(1)
          RemoteSubsumed.futured
        } else {
          if (DEBUG) println(s"[${Thread.currentThread().getName}] retrying contended remote trySubsume $this to $lockedNewParent")
          remoteTrySubsume(lockedNewParent)
        }
      case Self =>
        // not safe because held by different thread which could concurrently unlock and deallocate?
        if(tryLocalAddRefs(1)) {
          if (DEBUG) println(s"[${Thread.currentThread().getName}] remote trySubsume $this to $lockedNewParent blocked; added remote connection establishment reference and dropping temporary remote parameter reference")
          lockedNewParent.localSubRefs(1)
          Future.successful(RemoteBlocked(this))
        } else {
          if (DEBUG) println(s"[${Thread.currentThread().getName}] remote trySubsume $this to $lockedNewParent failed to deallocation race; dropping temporary remote parameter reference")
          lockedNewParent.localSubRefs(1)
          RemoteGCd.futured
        }
      case host.dummy =>
        if (DEBUG) println(s"[${Thread.currentThread().getName}] remote trySubsume $this to $lockedNewParent failed to deallocation race; dropping temporary remote parameter reference")
        lockedNewParent.localSubRefs(1)
        RemoteGCd.futured
      case parent =>
        parent.trySubsume0(0, lockedNewParent).flatMap {
          case Successful0(failedRefChanges) =>
            val finalFailedRefChanges = failedRefChanges + trySwap(parent, lockedNewParent) + 1
            if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning trySubsume success to remote, correcting $finalFailedRefChanges failed ref changes to $lockedNewParent (includes temporary remote parameter reference)")
            lockedNewParent.localSubRefs(finalFailedRefChanges)
            RemoteSubsumed.futured
          case Blocked0(failedRefChanges, newRoot) =>
            val finalFailedRefChanges = failedRefChanges + trySwap(parent, newRoot)
            if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning trySubsume blocked under $newRoot to remote, correcting $finalFailedRefChanges (retaining thread reference for remote connection establishment) and dropping temporary remote parameter reference on $lockedNewParent")
            if(finalFailedRefChanges != 0) newRoot.localSubRefs(finalFailedRefChanges)
            lockedNewParent.localSubRefs(1)
            Future.successful(RemoteBlocked(newRoot))
          case GarbageCollected0 =>
            if (DEBUG) println(s"[${Thread.currentThread().getName}] retrying remote trySubsume $this after parent was concurrently deallocated")
            this.asInstanceOf[SubsumableLock].remoteTrySubsume(lockedNewParent)
        }(FullMVEngine.notWorthToMoveToTaskpool)
    }
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
