package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.{GUID, TryLockResult}

object SubsumableLockLocalClone {
  def apply(subsumableLock: SubsumableLock): SubsumableLock = {
    val localProxy = new SubsumableLockMirror(subsumableLock)
    val clonedProxy = new SubsumableLockMirrorProxy {
      override def spinOnce(backoff: Long): GUID = localProxy.spinOnce(backoff)
      override def unlock(): Unit = localProxy.unlock()
      override def trySubsume(lockedNewParent: TryLockResult): Boolean = {
        val TryLockResult(success, newLocalParent, globalRoot) = lockedNewParent
        localProxy.trySubsume(TryLockResult(success, SubsumableLockLocalClone(newLocalParent), globalRoot))
      }
      override def tryLock(): (Boolean, GUID) = localProxy.tryLock()
      override def subsume(lockedNewParent: TryLockResult): Unit = {
        val TryLockResult(success, newLocalParent, globalRoot) = lockedNewParent
        localProxy.subsume(TryLockResult(success, SubsumableLockLocalClone(newLocalParent), globalRoot))
      }
      override def lock(): GUID = localProxy.lock()
      override def getLockedRoot: Option[GUID] = localProxy.getLockedRoot
    }
    new SubsumableLockReflection(clonedProxy)
  }
}
