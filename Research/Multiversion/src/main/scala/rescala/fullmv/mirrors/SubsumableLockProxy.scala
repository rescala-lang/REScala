package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.SubsumableLock

import scala.concurrent.Future

sealed trait RemoteTryLockResult
case class RemoteLocked(newRoot: SubsumableLock) extends RemoteTryLockResult

sealed trait RemoteTrySubsumeResult
case object RemoteSubsumed extends RemoteTrySubsumeResult
case class RemoteBlocked(newRoot: SubsumableLock) extends RemoteTrySubsumeResult with RemoteTryLockResult
case object RemoteGCd extends RemoteTrySubsumeResult with RemoteTryLockResult

trait SubsumableLockProxy {
  def getLockedRoot: Future[Option[Host.GUID]]
  def remoteTryLock(): Future[RemoteTryLockResult]
  def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[RemoteTrySubsumeResult]
  def remoteAsyncUnlock(): Unit
  def asyncRemoteRefDropped(): Unit
}
