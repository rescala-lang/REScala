package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.FullMVEngine
import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.{LockStateResult0, SubsumableLock}

import scala.concurrent.Future
import scala.concurrent.duration.Duration

object SubsumableLockLocalClone {
  def apply(subsumableLock: SubsumableLock, reflectionHost: SubsumableLockHost, fakeDelay: Duration = Duration.Zero): SubsumableLock = {
    val mirrorHost = subsumableLock.host
    val localProxy: SubsumableLockProxy = subsumableLock
    val remoteProxy = new SubsumableLockLocalCloneProxy(fakeDelay, mirrorHost, localProxy, reflectionHost)
    reflectionHost.getCachedOrReceiveRemoteWithReference(subsumableLock.guid, remoteProxy)
  }
}

class SubsumableLockLocalCloneProxy(fakeDelay: Duration, mirrorHost: SubsumableLockHost, localProxy: SubsumableLockProxy, reflectionHost: SubsumableLockHost) extends SubsumableLockProxy {
  override def getLockedRoot: Future[LockStateResult0] = FakeDelayer.future(fakeDelay, localProxy.getLockedRoot)
  override def remoteTryLock(): Future[RemoteTryLockResult] = FakeDelayer.future(fakeDelay, localProxy.remoteTryLock().map {
    case RemoteLocked(newParent: SubsumableLock) => RemoteLocked(SubsumableLockLocalClone(newParent, reflectionHost, fakeDelay))
    case RemoteBlocked(newParent: SubsumableLock) => RemoteBlocked(SubsumableLockLocalClone(newParent, reflectionHost, fakeDelay))
    case RemoteGCd => RemoteGCd
  }(FullMVEngine.notWorthToMoveToTaskpool))
  override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[RemoteTrySubsumeResult] = FakeDelayer.future(fakeDelay,  localProxy.remoteTrySubsume(SubsumableLockLocalClone(lockedNewParent, mirrorHost, fakeDelay)).map {
    case RemoteSubsumed => RemoteSubsumed
    case RemoteBlocked(newParent: SubsumableLock) => RemoteBlocked(SubsumableLockLocalClone(newParent, reflectionHost, fakeDelay))
    case RemoteGCd => RemoteGCd
  }(FullMVEngine.notWorthToMoveToTaskpool))
  override def remoteUnlock(): Unit = FakeDelayer.async(fakeDelay, localProxy.remoteUnlock())
  override def asyncRemoteRefDropped(): Unit = FakeDelayer.async(fakeDelay, localProxy.asyncRemoteRefDropped())
}
