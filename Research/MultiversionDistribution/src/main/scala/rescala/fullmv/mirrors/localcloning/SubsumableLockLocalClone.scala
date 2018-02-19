package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.FullMVEngine
import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.{LockStateResult0, SubsumableLock}

import scala.concurrent.Future

object SubsumableLockLocalClone {
  def apply(subsumableLock: SubsumableLock, reflectionHost: SubsumableLockHost): SubsumableLock = {
    val mirrorHost = subsumableLock.host
    val localProxy: SubsumableLockProxy = subsumableLock
    val remoteProxy = new SubsumableLockLocalCloneProxy(mirrorHost, localProxy, reflectionHost)
    reflectionHost.getCachedOrReceiveRemoteWithReference(subsumableLock.guid, remoteProxy)
  }
}

class SubsumableLockLocalCloneProxy(mirrorHost: SubsumableLockHost, localProxy: SubsumableLockProxy, reflectionHost: SubsumableLockHost) extends SubsumableLockProxy {
  override def getLockedRoot: Future[LockStateResult0] = localProxy.getLockedRoot
  override def remoteTryLock(): Future[RemoteTryLockResult] = localProxy.remoteTryLock().map {
    case RemoteLocked(newParent: SubsumableLock) => RemoteLocked(SubsumableLockLocalClone(newParent, reflectionHost))
    case RemoteBlocked(newParent: SubsumableLock) => RemoteBlocked(SubsumableLockLocalClone(newParent, reflectionHost))
    case RemoteGCd => RemoteGCd
  }(FullMVEngine.notWorthToMoveToTaskpool)
  override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[RemoteTrySubsumeResult] = {
    localProxy.remoteTrySubsume(SubsumableLockLocalClone(lockedNewParent, mirrorHost)).map {
      case RemoteSubsumed => RemoteSubsumed
      case RemoteBlocked(newParent: SubsumableLock) => RemoteBlocked(SubsumableLockLocalClone(newParent, reflectionHost))
      case RemoteGCd => RemoteGCd
    }(FullMVEngine.notWorthToMoveToTaskpool)
  }
  override def remoteAsyncUnlock(): Unit = localProxy.remoteAsyncUnlock()
  override def asyncRemoteRefDropped(): Unit = localProxy.asyncRemoteRefDropped()
}
