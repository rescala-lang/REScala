package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.SubsumableLock

trait SubsumableLockMirrorProxy {
  def subsume(lockedNewParent: SubsumableLock.TryLockResult): Unit
  def unlock(): Unit
  def getLockedRoot: Option[Host.GUID]
  def tryLock(): (Boolean, Host.GUID)
  def lock(): Host.GUID
  def spinOnce(backoff: Long): Host.GUID
  def trySubsume(lockedNewParent: SubsumableLock.TryLockResult): Boolean
}
