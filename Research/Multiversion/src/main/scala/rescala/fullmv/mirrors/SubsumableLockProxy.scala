package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.SubsumableLock

trait SubsumableLockProxy {
  // used for assertions only
  def getLockedRoot: Option[Host.GUID]
  def tryLock(): SubsumableLock.TryLockResult
  def lock(): SubsumableLock
  def spinOnce(backoff: Long): SubsumableLock
  // if failed, returns Some(newParent) that should be used as new Parent
  // if successful, returns None; lockedNewParent.newParent should be used as newParent.
  // newParent is not a uniform part of all possible return values to avoid establishing unnecessary back-and-forth remote paths
  def trySubsume(lockedNewParent: SubsumableLock): Option[SubsumableLock]
  def subsume(lockedNewParent: SubsumableLock): Unit
  def unlock(): SubsumableLock
}
