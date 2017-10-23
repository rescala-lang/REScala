package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.SubsumableLock

import scala.concurrent.Future

trait SubsumableLockProxy {
  def getLockedRoot: Future[Option[Host.GUID]]
  def remoteLock(): Future[SubsumableLock]
  def remoteSpinOnce(backoff: Long): Future[SubsumableLock]
  def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]]
  def remoteAsyncUnlock(): Unit
  def asyncRemoteRefDropped(): Unit
}
