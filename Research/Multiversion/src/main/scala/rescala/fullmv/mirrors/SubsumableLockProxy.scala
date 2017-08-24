package rescala.fullmv.mirrors

import scala.concurrent.Future

import rescala.fullmv.sgt.synchronization.SubsumableLock

trait SubsumableLockProxy {
  // used for assertions only
  def getLockedRoot: Future[Option[Host.GUID]]
  def tryLock(): Future[SubsumableLock.TryLockResult]
  def lock(): Future[SubsumableLock]
  def spinOnce(backoff: Long): Future[SubsumableLock]
  // if failed, returns Some(newParent) that should be used as new Parent
  // if successful, returns None; lockedNewParent.newParent should be used as newParent.
  // newParent is not a uniform part of all possible return values to avoid establishing unnecessary back-and-forth remote paths
  def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]]
  def subsume(lockedNewParent: SubsumableLock): Future[Unit]
  def unlock(): Future[SubsumableLock]
}
