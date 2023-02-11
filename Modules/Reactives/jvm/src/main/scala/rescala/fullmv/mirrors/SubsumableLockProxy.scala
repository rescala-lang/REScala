package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.{LockStateResult0, SubsumableLock}

import scala.concurrent.Future

sealed trait RemoteTryLockResult
case class RemoteLocked(newRoot: SubsumableLock) extends RemoteTryLockResult

sealed trait RemoteTrySubsumeResult
case object RemoteSubsumed extends RemoteTrySubsumeResult {
  val futured: Future[RemoteSubsumed.type] = Future.successful(this)
}
case class RemoteBlocked(newRoot: SubsumableLock) extends RemoteTrySubsumeResult with RemoteTryLockResult
case object RemoteGCd extends RemoteTrySubsumeResult with RemoteTryLockResult {
  val futured: Future[RemoteGCd.type] = Future.successful(this)
}

trait SubsumableLockProxy {
  def getLockedRoot: Future[LockStateResult0]
  // result will have one temporary remote parameter reference for the caller to receive.
  def remoteTryLock(): Future[RemoteTryLockResult]
  final def remoteTryLockNoTail(): Future[RemoteTryLockResult] = remoteTryLock()
  // parameter has one temporary remote parameter reference counted, which will be cleared by this call.
  // result will have one temporary remote parameter reference for the caller to receive.
  def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[RemoteTrySubsumeResult]
  final def remoteTrySubsumeNoTail(lockedNewParent: SubsumableLock): Future[RemoteTrySubsumeResult] =
    remoteTrySubsume(lockedNewParent)
  def remoteUnlock(): Unit
  def asyncRemoteRefDropped(): Unit
}
