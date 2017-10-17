package rescala.fullmv.mirrors.localcloning

import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.transmitter.ReactiveTransmittable

import scala.concurrent.Future

object SubsumableLockLocalClone {
  def apply(subsumableLock: SubsumableLock, reflectionHost: SubsumableLockHost): SubsumableLock = {
    val mirrorHost = subsumableLock.host
    reflectionHost.getCachedOrReceiveRemote(subsumableLock.guid) { cacheNow =>
      val localProxy: SubsumableLockProxy = subsumableLock
      val remoteProxy = new SubsumableLockLocalCloneProxy(mirrorHost, localProxy, reflectionHost)
      val instance = new SubsumableLockReflection(reflectionHost, subsumableLock.guid, remoteProxy)
      cacheNow(instance)
      instance
    }
  }
}

class SubsumableLockLocalCloneProxy(mirrorHost: SubsumableLockHost, localProxy: SubsumableLockProxy, reflectionHost: SubsumableLockHost) extends SubsumableLockProxy {
  override def getLockedRoot: Future[Option[Host.GUID]] = localProxy.getLockedRoot
  override def lock(): Future[SubsumableLock] = localProxy.lock().map(SubsumableLockLocalClone(_, reflectionHost))(ReactiveTransmittable.notWorthToMoveToTaskpool)
  override def spinOnce(backoff: Long): Future[SubsumableLock] = localProxy.spinOnce(backoff).map(SubsumableLockLocalClone(_, reflectionHost))(ReactiveTransmittable.notWorthToMoveToTaskpool)
  override def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = localProxy.trySubsume(SubsumableLockLocalClone(lockedNewParent, mirrorHost)).map(_.map(SubsumableLockLocalClone(_, reflectionHost)))(ReactiveTransmittable.notWorthToMoveToTaskpool)
  override def unlock(): Unit = localProxy.unlock()
  override def remoteRefDropped(): Unit = localProxy.remoteRefDropped()
}
