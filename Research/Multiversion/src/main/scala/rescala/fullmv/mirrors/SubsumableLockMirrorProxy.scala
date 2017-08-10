package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.GUID

trait SubsumableLockMirrorProxy {
  def subsume(lockedNewParent: SubsumableLock.TryLockResult): Unit
  def unlock(): Unit
  def getLockedRoot: Option[GUID]
  def tryLock(): (Boolean, GUID)
  def lock(): GUID
  def spinOnce(backoff: Long): GUID
  def trySubsume(lockedNewParent: SubsumableLock.TryLockResult): Boolean
}
