package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.transmitter.ReactiveTransmittable

import scala.concurrent.Future

object SubsumableLockLocalClone {
  def apply(subsumableLock: SubsumableLock, reflectionHost: SubsumableLockHost): SubsumableLock = {
    subsumableLock.localAddRefs(1)
    val mirrorHost = subsumableLock.host
    reflectionHost.getCachedOrReceiveRemote(subsumableLock.guid, { cacheNow =>
      val localProxy: SubsumableLockProxy = subsumableLock
      val remoteProxy = new SubsumableLockLocalCloneProxy(mirrorHost, localProxy, reflectionHost)
      val instance = new SubsumableLockReflection(reflectionHost, subsumableLock.guid, remoteProxy)
      cacheNow(instance)
      instance
    }, subsumableLock.asyncRemoteRefDropped())
  }
}

class SubsumableLockLocalCloneProxy(mirrorHost: SubsumableLockHost, localProxy: SubsumableLockProxy, reflectionHost: SubsumableLockHost) extends SubsumableLockProxy {
  override def getLockedRoot: Future[Option[Host.GUID]] = localProxy.getLockedRoot
  override def remoteLock(): Future[SubsumableLock] = localProxy.remoteLock().map(SubsumableLockLocalClone(_, reflectionHost))(ReactiveTransmittable.notWorthToMoveToTaskpool)
  override def remoteSpinOnce(backoff: Long): Future[SubsumableLock] = localProxy.remoteSpinOnce(backoff).map(SubsumableLockLocalClone(_, reflectionHost))(ReactiveTransmittable.notWorthToMoveToTaskpool)
  override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = localProxy.remoteTrySubsume(SubsumableLockLocalClone(lockedNewParent, mirrorHost)).map(_.map(SubsumableLockLocalClone(_, reflectionHost)))(ReactiveTransmittable.notWorthToMoveToTaskpool)
  override def remoteAsyncUnlock(): Unit = localProxy.remoteAsyncUnlock()
  override def asyncRemoteRefDropped(): Unit = localProxy.asyncRemoteRefDropped()
}
