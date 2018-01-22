package rescala.fullmv.mirrors

import rescala.fullmv.FullMVEngine
import rescala.fullmv.mirrors.Host.GUID
import rescala.fullmv.sgt.synchronization.SubsumableLock.DEBUG
import rescala.fullmv.sgt.synchronization._

import scala.concurrent.Future

class SubsumableLockReflection(override val host: SubsumableLockHost, override val guid: Host.GUID, val proxy: SubsumableLockProxy) extends SubsumableLock {
  override def getLockedRoot: Future[Option[GUID]] = proxy.getLockedRoot
  override def tryLock0(hopCount: Int): Future[TryLockResult0] = {
    if(tryLocalAddRefs(1)) {
      proxy.remoteTryLock().map { remoteRes =>
        val res = remoteRes match {
          case RemoteLocked(lock) =>
            if (lock == this) {
              if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this locked remotely; ${hopCount + 1} new refs")
              if(hopCount > 0) localAddRefs(hopCount)
            } else {
              if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this locked new remote parent $lock, passing ${hopCount + 1} new refs")
              lock.localAddRefs(hopCount + 1)
            }
            Locked0(0, lock)
          case RemoteBlocked(lock) =>
            if (lock == this) {
              if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this blocked remotely; $hopCount new refs")
              if (hopCount > 0) localAddRefs(hopCount)
            } else {
              if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this blocked on new remote parent $lock, passing ${hopCount + 1} new refs")
              lock.localAddRefs(hopCount + 1)
            }
            Locked0(0, lock)
          case RemoteGCd =>
            throw new AssertionError(s"since $this is holding a local reference to the remote $proxy, the remote instance should be prevented from deallocation?")
        }
        localSubRefs(1)
        res
      }(FullMVEngine.notWorthToMoveToTaskpool)
    } else {
      GarbageCollected0.futured
    }
  }

  override def trySubsume0(hopCount: Int, lockedNewParent: SubsumableLock): Future[TrySubsumeResult0] = {
    if(lockedNewParent == this) {
      assert(lockedNewParent eq this, s"instance caching broken? $this came into contact with different reflection of same origin on same host")
      if (DEBUG) println(s"[${Thread.currentThread().getName}] trySubsume $this to itself reentrant success; $hopCount new refs")
      // safe because locked by the current thread and thus cannot be deallocated
      if(hopCount > 0) localAddRefs(hopCount)
      Successful0.zeroFutured
    } else if(tryLocalAddRefs(1)) {
      proxy.remoteTrySubsume(lockedNewParent).map{ remoteRes =>
        val res = remoteRes match {
          case RemoteSubsumed =>
            if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this remote trySubsume succeeded, sending ${hopCount + 1} new refs to $lockedNewParent")
            lockedNewParent.localAddRefs(hopCount + 1)
            Successful0(0)
          case RemoteBlocked(newParent) =>
            if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this remote trySubsume failed, passing $hopCount new refs to $newParent")
            if(hopCount > 0) localAddRefs(hopCount)
            Blocked0(0, newParent)
          case RemoteGCd =>
            throw new AssertionError(s"since $this is holding a local reference to the remote $proxy, the remote instance should be prevented from deallocation?")
        }
        localSubRefs(1)
        res
      }(FullMVEngine.notWorthToMoveToTaskpool)
    } else {
      GarbageCollected0.futured
    }
  }

  override def asyncUnlock0(): Unit = proxy.remoteAsyncUnlock()
  override def remoteAsyncUnlock(): Unit = proxy.remoteAsyncUnlock()
  override def remoteTryLock(): Future[RemoteTryLockResult] = proxy.remoteTryLock()
  override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[RemoteTrySubsumeResult] = proxy.remoteTrySubsume(lockedNewParent)

  override protected def dumped(): Unit = {
    if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this no refs remaining, deallocating and dropping remote reference")
    proxy.asyncRemoteRefDropped()
  }

  override def toString: String = {
    val refs = refCount.get()
    s"SubsumableLockReflection($guid on $host, ${if(refs <= 0) "gc'd" else refs + " refs"})"
  }
}
