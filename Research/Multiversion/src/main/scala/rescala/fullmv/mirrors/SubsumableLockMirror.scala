package rescala.fullmv.mirrors

import java.util.concurrent.atomic.AtomicReference

import rescala.fullmv.sgt.synchronization.SubsumableLock
import rescala.fullmv.sgt.synchronization.SubsumableLock.TryLockResult

class SubsumableLockMirror(initialLocalLeaf: SubsumableLock) extends SubsumableLockMirrorProxy {
  val localLeaf = new AtomicReference[SubsumableLock](initialLocalLeaf)
  override def subsume(lockedNewParent: SubsumableLock.TryLockResult): Unit = {
    localLeaf.getAndSet(lockedNewParent.newParent).subsume(lockedNewParent)
  }

  override def unlock(): Unit = localLeaf.get.unlock()

  override def getLockedRoot: Option[Host.GUID] = localLeaf.get.getLockedRoot

  override def tryLock(): (Boolean, Host.GUID) = {
    val v = localLeaf.get
    val TryLockResult(success, newParent, root) = v.tryLock()
    localLeaf.compareAndSet(v, newParent)
    (success, root)
  }

  override def lock(): Host.GUID = {
    val v = localLeaf.get()
    val TryLockResult(_, newParent, root) = v.lock()
    localLeaf.compareAndSet(v, newParent)
    root
  }

  override def spinOnce(backoff: Long): Host.GUID = {
    val v = localLeaf.get()
    val TryLockResult(_, newParent, root) = v.spinOnce(backoff)
    localLeaf.compareAndSet(v, newParent)
    root
  }

  override def trySubsume(lockedNewParent: SubsumableLock.TryLockResult): Boolean = {
    val v = localLeaf.get()
    v.trySubsume(lockedNewParent) match {
      case None =>
        localLeaf.compareAndSet(v, lockedNewParent.newParent)
        true
      case Some(newParent) =>
        localLeaf.compareAndSet(v, newParent)
        false
    }
  }
}
