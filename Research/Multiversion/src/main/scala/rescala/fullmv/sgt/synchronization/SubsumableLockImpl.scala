package rescala.fullmv.sgt.synchronization

import java.util.concurrent.atomic.AtomicReference

import rescala.fullmv.sgt.synchronization.SubsumableLock._

import scala.annotation.elidable

class SubsumableLockImpl extends SubsumableLock {
  var locked: Option[Any] = None
  val parent = new AtomicReference[SubsumableLock](this)

  @elidable(elidable.ASSERTION)
  override def isLockedRoot(key: Any): Boolean = synchronized { parent.get() == this && locked.contains(key) }

  override def getLockedRoot(key: Any): Option[SubsumableLock] = {
    synchronized {
      val synchronP = parent.get()
      if (synchronP == this) {
        Left(if(locked.contains(key)) Some(this) else None)
      } else {
        Right(synchronP)
      }
    } match {
      case Left(result) => result
      case Right(parent) =>
        parent.getLockedRoot(key)
    }
  }

  override def tryLock(key: Any): TryLockResult = {
    val p = parent.get()
    val res = if(p == this) synchronized {
      val synchronP = parent.get()
      val success = if(synchronP != this) {
        false
      } else if(locked.isEmpty) {
        locked = Some(key)
        true
      } else if(locked.get == key) {
        true
      } else {
        false
      }
      TryLockResult(success, synchronP)
    } else {
      TryLockResult(success = false, p)
    }
    if(res.newRoot == this) {
      res
    } else {
      val parentRes = res.newRoot.tryLock(key)
      parent.compareAndSet(res.newRoot, parentRes.newRoot) // ignore result; if it failed, someone else already compressed the path
      parentRes
    }
  }

  override def lock(key: Any): SubsumableLock = {
    val p = parent.get()
    val nextP = if(p == this) synchronized {
      while(parent.get() == this && locked.isDefined && locked.get != key) {
        wait()
      }
      val nextP = parent.get()
      if(nextP == this) locked = Some(key)
      nextP
    } else {
      p
    }
    if(nextP == this) {
      this
    } else {
      val newRoot = nextP.lock(key)
      parent.compareAndSet(nextP, newRoot) // ignore result; if it failed, someone else already compressed the path
      newRoot
    }
  }

  override def unlock(key: Any): Unit = synchronized {
    assert(parent.get() == this, "unlock called on subsumed")
    assert(locked.contains(key), "unlock by unauthorized key")
    locked = None
    notifyAll()
  }

  override def subsume(subsumableLock: SubsumableLock): SubsumableLock = synchronized {
    assert(parent.get() == this, "subsume on non-root")
    assert(locked.isDefined, "subsume on unlocked node")
    assert(subsumableLock.isLockedRoot(locked.get), "subsume partner not locked root")
    parent.set(subsumableLock)
    locked = None
    notifyAll()
    subsumableLock
  }
}
