package rescala.parrp

import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec
import scala.collection.immutable.Queue

final class TurnLock() {

  private val owner: AtomicReference[Key] = new AtomicReference[Key]()
  private val shared: AtomicReference[Queue[Key]] = new AtomicReference[Queue[Key]](Queue())
  private var writeLock: Boolean = true

  def getOwner: Key = owner.get()
  def isWriteLock: Boolean = writeLock

  /** returns true if key owns the write lock */
  def isOwner(key: Key): Boolean = owner.get() eq key

  /**
   * locks this if it is free, returns the current owner (which is key, if locking succeeded)
   * does not check for shared access.
   */
  @tailrec
  def tryLock(key: Key, write: Boolean = true): Key = {
    if (owner.compareAndSet(null, key)) {
      key.addLock(this)
      writeLock = write
    }
    val current = owner.get()
    if (current eq null) tryLock(key)
    else current
  }

  @tailrec
  private def transform[T](v: AtomicReference[T])(f: T => T): Unit = {
    val old = v.get()
    val update = f(old)
    if (!v.compareAndSet(old, update)) transform(v)(f)
  }

  def share(key: Key): Unit = transform(shared)(_.enqueue(key))
  def acquired(key: Key): Key = {
    transform(shared) { q =>
      val (k, r) = q.dequeue
      assert(k == key, s"resolved await in wrong order got $k expected $key remaining $r")
      r
    }
    key
  }

  /** transfers the lock from the turn to the target. */
  def transfer(target: Key, oldOwner: Key, transferWriteSet: Boolean = false) = {
    // update locks back to read locks when transferring
    writeLock = transferWriteSet && writeLock
    // select the true target:
    // if there is no shared node, set target to null – free the lock
    // if a fallthrough exists always transfer the lock
    val trueTarget =
      if (!transferWriteSet && shared.get.isEmpty) null
      else target

    if (!owner.compareAndSet(oldOwner, trueTarget))
      assert(assertion = false, s"$this is held by $owner but tried to transfer by $oldOwner (to $target)")

    if (trueTarget ne null) trueTarget.addLock(this)
  }

}

