package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult

object SubsumableLockLocalClone {
  def localCloneTryLockResult(tryLockResult: TryLockResult, reflectionHost: SubsumableLockHost): TryLockResult = {
    val TryLockResult(success, newLocalParent, globalRoot) = tryLockResult
    TryLockResult(success, SubsumableLockLocalClone(newLocalParent, reflectionHost), globalRoot)
  }
  def apply(subsumableLock: SubsumableLock, reflectionHost: SubsumableLockHost): SubsumableLock = {
    val mirrorHost = subsumableLock.host
    reflectionHost.getCachedOrReceiveRemote(subsumableLock.guid) { cacheNow =>
      val localMirror: SubsumableLockMirrorProxy = new SubsumableLockMirror(subsumableLock)
      val mirrorProxy = new SubsumableLockMirrorProxy {
        override def spinOnce(backoff: Long): Host.GUID = localMirror.spinOnce(backoff)
        override def unlock(): Unit = localMirror.unlock()
        override def trySubsume(lockedNewParent: TryLockResult): Boolean = localMirror.trySubsume(localCloneTryLockResult(lockedNewParent, mirrorHost))
        override def tryLock(): (Boolean, Host.GUID) = localMirror.tryLock()
        override def subsume(lockedNewParent: TryLockResult): Unit = localMirror.subsume(localCloneTryLockResult(lockedNewParent, mirrorHost))
        override def lock(): Host.GUID = localMirror.lock()
        override def getLockedRoot: Option[Host.GUID] = localMirror.getLockedRoot
      }
      val instance = new SubsumableLockReflection(reflectionHost, subsumableLock.guid, mirrorProxy)
      cacheNow(instance) // TODO figure out garbage collection...
      instance
    }
  }
}
