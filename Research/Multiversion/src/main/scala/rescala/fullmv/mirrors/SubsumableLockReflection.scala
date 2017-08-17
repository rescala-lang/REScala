package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.SubsumableLock

trait SubsumableLockReflectionMethodsToProxy extends SubsumableLockProxy {
  val proxy: SubsumableLockProxy
  override def subsume(lockedNewParent: SubsumableLock): Unit = proxy.subsume(lockedNewParent)
  override def unlock(): SubsumableLock = proxy.unlock()
  override def getLockedRoot: Option[Host.GUID] = proxy.getLockedRoot
  override def tryLock(): SubsumableLock.TryLockResult = proxy.tryLock()
  override def lock(): SubsumableLock = proxy.lock()
  override def spinOnce(backoff: Long): SubsumableLock = proxy.spinOnce(backoff)
  override def trySubsume(lockedNewParent: SubsumableLock): Option[SubsumableLock] = proxy.trySubsume(lockedNewParent)
}

class SubsumableLockReflection(override val host: SubsumableLockHost, override val guid: Host.GUID, override val proxy: SubsumableLockProxy) extends SubsumableLock with SubsumableLockReflectionMethodsToProxy {
  override def trySubsume(lockedNewParent: SubsumableLock): Option[SubsumableLock] = {
    if(lockedNewParent == this) {
      assert(lockedNewParent eq this, s"instance caching broken? $this came into contact with different reflection of same origin on same host")
      None
    } else {
      super.trySubsume(lockedNewParent)
    }
  }

  override def toString: String = s"SubsumableLockReflection($guid on $host)"
}
