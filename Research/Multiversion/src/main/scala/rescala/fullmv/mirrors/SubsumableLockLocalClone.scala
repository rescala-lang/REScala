package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.{GUID, TryLockResult}

object SubsumableLockLocalClone {
  def localCloneTryLockResult(tryLockResult: TryLockResult): TryLockResult = {
    val TryLockResult(success, newLocalParent, globalRoot) = tryLockResult
    TryLockResult(success, SubsumableLockLocalClone(newLocalParent), globalRoot)
  }
  def apply(subsumableLock: SubsumableLock): SubsumableLock = {
    val localProxy = new SubsumableLockMirror(subsumableLock)
    val clonedProxy = new SubsumableLockMirrorProxy {
      override def spinOnce(backoff: Long): GUID = localProxy.spinOnce(backoff)
      override def unlock(): Unit = localProxy.unlock()
      override def trySubsume(lockedNewParent: TryLockResult): Boolean = localProxy.trySubsume(localCloneTryLockResult(lockedNewParent))
      override def tryLock(): (Boolean, GUID) = localProxy.tryLock()
      override def subsume(lockedNewParent: TryLockResult): Unit = localProxy.subsume(localCloneTryLockResult(lockedNewParent))
      override def lock(): GUID = localProxy.lock()
      override def getLockedRoot: Option[GUID] = localProxy.getLockedRoot
    }
    new SubsumableLockReflection(clonedProxy)
  }
}
