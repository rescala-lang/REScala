package reactives.fullmv.mirrors

import reactives.fullmv.FullMVUtil
import reactives.fullmv.mirrors.Host.DEBUG
import reactives.fullmv.sgt.synchronization.*

import scala.concurrent.Future

// TODO should be able to shortcut the localAddRefs(x)+failedRefChanges=1 into localAddRefs(x-1)
class SubsumableLockReflection(
    override val host: SubsumableLockHost,
    override val guid: Host.GUID,
    val proxy: SubsumableLockProxy
) extends SubsumableLock {
  override def getLockedRoot: Future[LockStateResult0] = proxy.getLockedRoot
  override def tryLock0(hopCount: Int): Future[TryLockResult0] = {
    if (Host.DEBUG) println(s"[${Thread.currentThread().getName}] $this sending remote tryLock request")
    proxy.remoteTryLock().map {
      case RemoteLocked(lock) =>
        if (lock eq this) { // note: This must not be an == comparison. The received instance may be a new remote reflection of the same original lock, while this instance was concurrently deallocated and cannot have new references added!
          if (Host.DEBUG)
            println(
              s"[${Thread.currentThread().getName}] $this locked remotely; adding $hopCount new refs (retaining connection establishment reference as thread reference)"
            )
          if (hopCount > 0) localAddRefs(hopCount)
        } else {
          if (Host.DEBUG)
            println(
              s"[${Thread.currentThread().getName}] $this locked remotely under new parent $lock, passing ${hopCount + 1} new refs"
            )
          lock.localAddRefs(hopCount + 1)
        }
        Locked0(0, lock)
      case RemoteBlocked(lock) =>
        if (lock eq this) { // note: This must not be an == comparison. The received instance may be a new remote reflection of the same original lock, while this instance was concurrently deallocated and cannot have new references added!
          if (Host.DEBUG)
            println(
              s"[${Thread.currentThread().getName}] $this blocked remotely; $hopCount new refs (retaining connection establishment reference as thread reference)"
            )
          if (hopCount > 0) localAddRefs(hopCount)
        } else {
          if (Host.DEBUG)
            println(
              s"[${Thread.currentThread().getName}] $this blocked remotely under new parent $lock, passing ${hopCount + 1} new refs (retaining connection establishment reference as thread reference)"
            )
          lock.localAddRefs(hopCount + 1)
        }
        Blocked0(0, lock)
      case RemoteGCd =>
        assert(this.refCount.get <= 0, s"remote was gc'd while $this still holds a reference")
        GarbageCollected0
    }(FullMVUtil.notWorthToMoveToTaskpool)
  }

  override def trySubsume0(hopCount: Int, lockedNewParent: SubsumableLock): Future[TrySubsumeResult0] = {
    if (lockedNewParent == this) {
      assert(
        lockedNewParent eq this,
        s"instance caching broken? $this came into contact with different reflection of same origin on same host"
      )
      if (DEBUG)
        println(
          s"[${Thread.currentThread().getName}] trySubsume $this to itself reentrant success; $hopCount new refs (includes new thread reference)"
        )
      // safe because locked by the current thread and thus cannot be deallocated
      if (hopCount > 0) localAddRefs(hopCount)
      Future.successful(Successful0(0))
    } else {
      if (Host.DEBUG)
        println(
          s"[${Thread.currentThread().getName}] $this sending remote trySubsume $lockedNewParent request, adding remote parameter transfer reference"
        )
      lockedNewParent.localAddRefs(1)
      proxy.remoteTrySubsume(lockedNewParent).map {
        case RemoteSubsumed =>
          if (Host.DEBUG)
            println(
              s"[${Thread.currentThread().getName}] $this remote trySubsume succeeded, sending ${hopCount + 1} new refs to $lockedNewParent"
            )
          lockedNewParent.localAddRefs(hopCount + 1)
          Successful0(0)
        case RemoteBlocked(newParent) =>
          if (newParent eq this) {
            if (Host.DEBUG)
              println(
                s"[${Thread.currentThread().getName}] $this remote trySubsume $lockedNewParent blocked, $hopCount new refs (retaining connection establishment reference as thread reference)"
              )
            if (hopCount > 0) newParent.localAddRefs(hopCount)
          } else {
            if (Host.DEBUG)
              println(
                s"[${Thread.currentThread().getName}] $this remote trySubsume $lockedNewParent blocked under new parent $newParent, passing ${hopCount + 1} refs (retaining connection establishment reference as thread reference)"
              )
            newParent.localAddRefs(hopCount + 1)
          }
          Blocked0(0, newParent)
        case RemoteGCd =>
          assert(this.refCount.get <= 0, s"remote was gc'd while $this still holds a reference")
          GarbageCollected0
      }(FullMVUtil.notWorthToMoveToTaskpool)
    }
  }

  override def asyncUnlock0(): Unit = {
    if (Host.DEBUG) println(s"[${Thread.currentThread().getName}] $this sending unlock request")
    proxy.remoteUnlock()
  }
  override def remoteUnlock(): Unit = {
    if (Host.DEBUG) println(s"[${Thread.currentThread().getName}] $this passing through unlock request")
    proxy.remoteUnlock()
  }
  override def remoteTryLock(): Future[RemoteTryLockResult] = {
    if (Host.DEBUG) println(s"[${Thread.currentThread().getName}] $this passing through tryLock request")
    proxy.remoteTryLock().map { res =>
      if (Host.DEBUG)
        println(s"[${Thread.currentThread().getName}] $this passing through tryLock result $res")
      res
    }(FullMVUtil.notWorthToMoveToTaskpool)
  }
  override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[RemoteTrySubsumeResult] = {
    if (Host.DEBUG)
      println(
        s"[${Thread.currentThread().getName}] $this passing through trySubsume $lockedNewParent, using parameter reference as connection establishment reference"
      )
    proxy.remoteTrySubsume(lockedNewParent).map { res =>
      if (Host.DEBUG)
        println(s"[${Thread.currentThread().getName}] $this passing through trySubsume $lockedNewParent result $res")
      res
    }(FullMVUtil.notWorthToMoveToTaskpool)
  }

  override protected def dumped(): Unit = {
    if (Host.DEBUG)
      println(
        s"[${Thread.currentThread().getName}] $this no refs remaining, deallocating and dropping remote reference"
      )
    proxy.asyncRemoteRefDropped()
  }

  override def toString: String = {
    val refs = refCount.get()
    s"SubsumableLockReflection($guid on $host, ${if (refs <= 0) "gc'd" else refs.toString + " refs"})"
  }
}
