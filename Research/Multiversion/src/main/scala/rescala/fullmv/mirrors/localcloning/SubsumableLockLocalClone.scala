package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.mirrors._
import rescala.fullmv.mirrors.localcloning.SubsumableLockLocalClone.localCloneTryLockResult
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult

object SubsumableLockLocalClone {
  def localCloneTryLockResult(tryLockResult: TryLockResult, reflectionHost: SubsumableLockHost): TryLockResult = {
    val TryLockResult(success, newLocalParent) = tryLockResult
    TryLockResult(success, SubsumableLockLocalClone(newLocalParent, reflectionHost))
  }
  def apply(subsumableLock: SubsumableLock, reflectionHost: SubsumableLockHost): SubsumableLock = {
    val mirrorHost = subsumableLock.host
    reflectionHost.getCachedOrReceiveRemote(subsumableLock.guid) { cacheNow =>
      val localProxy: SubsumableLockProxy = subsumableLock
      val remoteProxy = new SubsumableLockLocalCloneProxy(mirrorHost, localProxy, reflectionHost)
      val instance = new SubsumableLockReflection(reflectionHost, subsumableLock.guid, remoteProxy)
      cacheNow(instance) // TODO figure out garbage collection...
      instance
    }
  }
}

class SubsumableLockLocalCloneProxy(mirrorHost: SubsumableLockHost, localProxy: SubsumableLockProxy, reflectionHost: SubsumableLockHost) extends SubsumableLockProxy {
  override def spinOnce(backoff: Long): SubsumableLock = SubsumableLockLocalClone(localProxy.spinOnce(backoff), reflectionHost)
  override def unlock(): SubsumableLock = SubsumableLockLocalClone(localProxy.unlock(), reflectionHost)
  override def trySubsume(lockedNewParent: SubsumableLock): Option[SubsumableLock] = localProxy.trySubsume(SubsumableLockLocalClone(lockedNewParent, mirrorHost)).map(SubsumableLockLocalClone(_, reflectionHost))
  override def tryLock(): TryLockResult = localCloneTryLockResult(localProxy.tryLock(), reflectionHost)
  override def subsume(lockedNewParent: SubsumableLock): Unit = localProxy.subsume(SubsumableLockLocalClone(lockedNewParent, mirrorHost))
  override def lock(): SubsumableLock = SubsumableLockLocalClone(localProxy.lock(), reflectionHost)
  override def getLockedRoot: Option[Host.GUID] = localProxy.getLockedRoot
}
