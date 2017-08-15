package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult

class SubsumableLockReflection(override val host: SubsumableLockHost, override val guid: Host.GUID, val mirrorProxy: SubsumableLockMirrorProxy) extends SubsumableLock {
  override def subsume(lockedNewParent: SubsumableLock.TryLockResult): Unit = mirrorProxy.subsume(lockedNewParent)
  override def unlock(): Unit = mirrorProxy.unlock()
  override def getLockedRoot: Option[Host.GUID] = mirrorProxy.getLockedRoot
  override def tryLock(): SubsumableLock.TryLockResult = {
    val(success, root) = mirrorProxy.tryLock()
    TryLockResult(success, this, root)
  }
  override def lock(): SubsumableLock.TryLockResult = {
    val root = mirrorProxy.lock()
    TryLockResult(success = true, this, root)
  }
  override def spinOnce(backoff: Long): SubsumableLock.TryLockResult = {
    val root = mirrorProxy.spinOnce(backoff)
    TryLockResult(success = true, this, root)
  }
  override def trySubsume(lockedNewParent: SubsumableLock.TryLockResult): Option[SubsumableLock] = {
    val success = mirrorProxy.trySubsume(lockedNewParent)
    if(success) None else Some(this)
  }

  override def toString: String = s"SubsumableLockReflection($guid on $host)"
}
