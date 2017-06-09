package rescala.fullmv.sgt.synchronization

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.LockSupport

import rescala.fullmv.sgt.synchronization.SubsumableLock._

import scala.annotation.{elidable, tailrec}


sealed trait LockState
case object Unlocked extends LockState
case class Locked(key: Any) extends LockState
case class Subsumed(parent: SubsumableLock) extends LockState

class SubsumableLockImpl extends SubsumableLock {
  val DEBUG = false

  val state = new AtomicReference[LockState](Unlocked)

  @elidable(elidable.ASSERTION)
  override def isLockedRoot(key: Any): Boolean = state.get == Locked(key)

  override def getLockedRoot(key: Any): Option[SubsumableLock] = {
    state.get match {
      case Subsumed(parent) => parent.getLockedRoot(key)
      case Locked(`key`) => Some(this)
      case _ => None
    }
  }

  override def tryLock(key: Any): TryLockResult = {
    state.get match {
      case Subsumed(parent) =>
        val res = parent.tryLock(key)
        state.set(Subsumed(res.newRoot))
        res
      case Locked(owner) =>
        val success = key == owner
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: ${if(success) "reentrant trylock on " + this else s"trylock $this failed: locked by $owner"}")
        TryLockResult(success, this)
      case Unlocked =>
        val success = state.compareAndSet(Unlocked, Locked(key))
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: ${if(success) "trylocked" else "failed trylock to contention"} $this")
        TryLockResult(success, this)
    }
  }

  val waiters = new ConcurrentLinkedQueue[Thread]()
  @tailrec final override def lock(key: Any): SubsumableLock = {
    state.get match {
      case Subsumed(parent) =>
        val res = parent.lock(key)
        state.set(Subsumed(res))
        res
      case Locked(`key`) =>
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: reentrant lock on $this")
        this
      case Locked(owner) =>
        val thread = Thread.currentThread()
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: found $this locked by $owner")
        waiters.add(thread)

        while(waiters.peek() != thread || state.get == Locked(owner)) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: parking on $this")
          LockSupport.park(this)
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: unparked on $this")
        }

        waiters.remove()
        if(state.get.isInstanceOf[Subsumed]) {
          val peeked = waiters.peek()
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: $this was subsumed" + (if(peeked == null) ", no successor" else "-> also unparking successor "+peeked.getName))
          LockSupport.unpark(peeked)
        }
        lock(key)
      case Unlocked =>
        if(state.compareAndSet(Unlocked, Locked(key))) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: locked $this")
          this
        } else {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: lock attempt of $this failed due to contention")
          lock(key)
        }
    }
  }

  override def unlock(key: Any): Unit = synchronized {
    assert(state.get == Locked(key), s"unlock($key) called illegally on state $state")
    state.set(Unlocked)
    val peeked = waiters.peek()
    if(DEBUG) println(s"[${Thread.currentThread().getName}]: release $this, unparking $peeked")
    LockSupport.unpark(peeked)
  }

  override def subsume(subsumableLock: SubsumableLock): SubsumableLock = synchronized {
    assert(subsumableLock != this, s"trying to create endless loops, are you?")
    state.get match {
      case Subsumed(_) => throw new AssertionError("subsume on non-root")
      case Unlocked => throw new AssertionError("subsume on unlocked node")
      case Locked(key) =>
        assert(subsumableLock.isLockedRoot(key), "subsume partner not locked root")
        state.set(Subsumed(subsumableLock))
        val peeked = waiters.peek()
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: subsume $this to $subsumableLock, unparking " + (if(peeked == null) "none" else peeked.getName))
        LockSupport.unpark(peeked)
    }
    subsumableLock
  }
}
