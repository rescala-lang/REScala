package rescala.fullmv.mirrors

import rescala.fullmv.FullMVEngine
import rescala.fullmv.mirrors.Host.GUID
import rescala.fullmv.sgt.synchronization.SubsumableLock

import scala.concurrent.Future

class SubsumableLockReflection(override val host: SubsumableLockHost, override val guid: Host.GUID, val proxy: SubsumableLockProxy) extends SubsumableLock {
  override def getLockedRoot: Future[Option[GUID]] = proxy.getLockedRoot
  override def lock0(hopCount: Int, lastHopWasGCd: Boolean): Future[(Int, SubsumableLock)] = {
    proxy.remoteLock().map { res =>
      if(res == this) {
        val addHops = hopCount + (if(lastHopWasGCd) 1 else 0)
        if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this locked remotely; $addHops new refs")
        if(addHops > 0) localAddRefs(addHops)
      } else {
        val addHops = 1 + hopCount + (if(lastHopWasGCd) 1 else 0)
        if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this remote lock returned new parent $res, passing $addHops new refs")
        res.localAddRefs(addHops)
      }
      (0, res)
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }

  override def trySubsume0(hopCount: Int, lastHopWasGCd: Boolean, lockedNewParent: SubsumableLock): Future[(Int, Option[SubsumableLock])] = {
    if(lockedNewParent == this) {
      assert(lockedNewParent eq this, s"instance caching broken? $this came into contact with different reflection of same origin on same host")
      Future.successful((0, None))
    } else {
      proxy.remoteTrySubsume(lockedNewParent).map{ res =>
        val newParent = res.getOrElse(lockedNewParent)
        if(newParent == this) {
          val addHops = hopCount + (if(lastHopWasGCd) 1 else 0)
          if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this remote trySubsume failed, $addHops new refs")
          if(addHops > 0) localAddRefs(addHops)
        } else {
          val addHops = 1 + hopCount + (if(lastHopWasGCd) 1 else 0)
          if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this remote trySubsume succeeded, sending $res $addHops new refs")
          newParent.localAddRefs(addHops)
        }
        (0, res)}(FullMVEngine.notWorthToMoveToTaskpool)
    }
  }

  override def asyncUnlock0(): Unit = proxy.remoteAsyncUnlock()
  override def spinOnce0(backoff: Long): Future[(Int, SubsumableLock)] = proxy.remoteSpinOnce(backoff).map { res =>
    (0, res)
  }(FullMVEngine.notWorthToMoveToTaskpool)

  override def remoteAsyncUnlock(): Unit = proxy.remoteAsyncUnlock()
  override def remoteLock(): Future[SubsumableLock] = proxy.remoteLock()
  override def remoteSpinOnce(backoff: GUID): Future[SubsumableLock] = proxy.remoteSpinOnce(backoff)
  override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = proxy.remoteTrySubsume(lockedNewParent)


  override protected def dumped(): Unit = {
    if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this no refs remaining, deallocating and dropping remote reference")
    proxy.asyncRemoteRefDropped()
  }

  override def toString: String = {
    val refs = refCount.get()
    s"SubsumableLockReflection($guid on $host, ${if(refs <= 0) "gc'd" else refs + " refs"})"
  }
}
