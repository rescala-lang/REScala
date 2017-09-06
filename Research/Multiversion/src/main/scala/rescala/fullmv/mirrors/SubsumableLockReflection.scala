package rescala.fullmv.mirrors

import rescala.fullmv.sgt.synchronization.SubsumableLock

import scala.concurrent.Future

trait SubsumableLockReflectionMethodsToProxy extends SubsumableLockProxy {
  val proxy: SubsumableLockProxy
  override def subsume(lockedNewParent: SubsumableLock): Future[Unit] = proxy.subsume(lockedNewParent)
  override def unlock(): Future[SubsumableLock] = proxy.unlock()
  override def getLockedRoot: Future[Option[Host.GUID]] = proxy.getLockedRoot
  override def tryLock(): Future[SubsumableLock.TryLockResult] = proxy.tryLock()
  override def lock(): Future[SubsumableLock] = proxy.lock()
  override def spinOnce(backoff: Long): Future[SubsumableLock] = proxy.spinOnce(backoff)
  override def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = proxy.trySubsume(lockedNewParent)
}

class SubsumableLockReflection(override val host: SubsumableLockHost, override val guid: Host.GUID, override val proxy: SubsumableLockProxy) extends SubsumableLock with SubsumableLockReflectionMethodsToProxy {
  override def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = {
    if(lockedNewParent == this) {
      assert(lockedNewParent eq this, s"instance caching broken? $this came into contact with different reflection of same origin on same host")
      Future.successful(None)
    } else {
      super.trySubsume(lockedNewParent)
    }
  }

  override def toString: String = s"SubsumableLockReflection($guid on $host)"
}
