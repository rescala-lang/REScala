package rescala.fullmv.mirrors

import rescala.fullmv.mirrors.Host.GUID
import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.transmitter.ReactiveTransmittable

import scala.concurrent.Future

class SubsumableLockReflection(override val host: SubsumableLockHost, override val guid: Host.GUID, val proxy: SubsumableLockProxy) extends SubsumableLock {
  override def getLockedRoot: Future[Option[GUID]] = proxy.getLockedRoot
  override def lock(hopCount: Int): Future[(Int, SubsumableLock)] = {
    proxy.lock().map { res =>
      if(res == this) {
        addRefs(hopCount)
      } else {
        res.addRefs(hopCount + 1)
      }
      (0, res)
    }(ReactiveTransmittable.notWorthToMoveToTaskpool)
  }
  override def spinOnce(hopCount: Int, backoff: Long): Future[(Int, SubsumableLock)] = proxy.spinOnce(backoff).map { res =>
    if(res == this) {
      addRefs(hopCount)
    } else {
      res.addRefs(hopCount + 1)
    }
    (0, res)
  }(ReactiveTransmittable.notWorthToMoveToTaskpool)
  override def trySubsume(hopCount: Int, lockedNewParent: SubsumableLock): Future[(Int, Option[SubsumableLock])] = {
    if(lockedNewParent == this) {
      assert(lockedNewParent eq this, s"instance caching broken? $this came into contact with different reflection of same origin on same host")
      Future.successful((0, None))
    } else {
      proxy.trySubsume(lockedNewParent).map{ res =>
        val newParent = res.getOrElse(lockedNewParent)
        if(newParent == this) {
          addRefs(hopCount)
        } else {
          newParent.addRefs(hopCount + 1)
        }
        (0, res)}(ReactiveTransmittable.notWorthToMoveToTaskpool)
    }
  }

  override def unlock(): Unit = proxy.unlock()

  override def lock(): Future[SubsumableLock] = proxy.lock()
  override def spinOnce(backoff: GUID): Future[SubsumableLock] = proxy.spinOnce(backoff)
  override def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = proxy.trySubsume(lockedNewParent)


  override protected def dumped(): Unit = proxy.remoteRefDropped()

  override def toString: String = {
    val refs = refCount.get()
    s"SubsumableLockReflection($guid on $host, ${if(refs <= 0) "gc'd" else refs + " refs"})"
  }
}
