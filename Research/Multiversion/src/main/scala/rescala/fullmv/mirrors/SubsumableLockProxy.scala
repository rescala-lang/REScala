package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.SubsumableLock

import scala.concurrent.Future

trait SubsumableLockProxy {
  def getLockedRoot: Future[Option[Host.GUID]]
  def lock(): Future[SubsumableLock]
  def spinOnce(backoff: Long): Future[SubsumableLock]
  def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]]
  def unlock(): Unit
  def remoteRefDropped(): Unit
}
