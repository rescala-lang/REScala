package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.mirrors._
import rescala.fullmv.mirrors.localcloning.SubsumableLockLocalClone.localCloneTryLockResult
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult
import rescala.fullmv.transmitter.ReactiveTransmittable

import scala.concurrent.Future

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
  override def spinOnce(backoff: Long): Future[SubsumableLock] = localProxy.spinOnce(backoff).map(SubsumableLockLocalClone(_, reflectionHost))(ReactiveTransmittable.notWorthToMoveToTaskpool)
  override def unlock(): Future[SubsumableLock] = localProxy.unlock().map(SubsumableLockLocalClone(_, reflectionHost))(ReactiveTransmittable.notWorthToMoveToTaskpool)
  override def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = localProxy.trySubsume(SubsumableLockLocalClone(lockedNewParent, mirrorHost)).map(_.map(SubsumableLockLocalClone(_, reflectionHost)))(ReactiveTransmittable.notWorthToMoveToTaskpool)
  override def tryLock(): Future[TryLockResult] = localProxy.tryLock().map(localCloneTryLockResult(_, reflectionHost))(ReactiveTransmittable.notWorthToMoveToTaskpool)
  override def subsume(lockedNewParent: SubsumableLock): Future[Unit] = localProxy.subsume(SubsumableLockLocalClone(lockedNewParent, mirrorHost))
  override def lock(): Future[SubsumableLock] = localProxy.lock().map(SubsumableLockLocalClone(_, reflectionHost))(ReactiveTransmittable.notWorthToMoveToTaskpool)
  override def getLockedRoot: Future[Option[Host.GUID]] = localProxy.getLockedRoot
}
