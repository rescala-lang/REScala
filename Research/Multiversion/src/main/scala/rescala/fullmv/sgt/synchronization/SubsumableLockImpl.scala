package rescala.fullmv.sgt.synchronization

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.LockSupport

import rescala.fullmv.sgt.synchronization.SubsumableLock._

import scala.annotation.tailrec

class SubsumableLockImpl extends SubsumableLock {
  Self =>
  val DEBUG = false

  val state = new AtomicReference[SubsumableLock](null)

  override def getLockedRoot: Option[SubsumableLock] = {
    state.get match {
      case null => None
      case Self => Some(this)
      case parent => parent.getLockedRoot
    }
  }

  override def tryLock(): TryLockResult = {
    state.get match {
      case null =>
        val success = state.compareAndSet(null, Self)
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: ${if(success) "trylocked" else "failed trylock to contention"} $this")
        TryLockResult(success, this)
      case Self =>
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: trylock $this blocked")
        TryLockResult(success = false, this)
      case parent =>
        val res = parent.tryLock()
        state.set(res.newRoot)
        res
    }
  }

  val waiters = new ConcurrentLinkedQueue[Thread]()
  @tailrec final override def lock(): SubsumableLock = {
    state.get match {
      case null =>
        if(state.compareAndSet(null, Self)) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: locked $this")
          this
        } else {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: retrying contended lock attempt of $this")
          lock()
        }
      case Self =>
        val thread = Thread.currentThread()
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: waiting for lock on $this")
        waiters.add(thread)

        while(waiters.peek() != thread || state.get == Self) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: parking on $this")
          LockSupport.park(this)
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: unparked on $this")
        }

        waiters.remove()
        val s = state.get
        if(s != null && s != Self) {
          val peeked = waiters.peek()
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: previous owner subsumed $this, moving on " + (if(peeked == null) "without successor" else "after also unparking successor "+peeked.getName))
          LockSupport.unpark(peeked)
        }
        lock()
      case parent =>
        val res = parent.lock()
        state.set(res)
        res
    }
  }

  override def unlock(): Unit = synchronized {
    assert(state.get != null, s"unlock attempt on non-locked $this")
    assert(state.get == Self, s"unlock attempt on subsumed $this")
    if(!state.compareAndSet(Self, null)) throw new AssertionError(s"$this unlock failed due to contention!?")
    val peeked = waiters.peek()
    if(DEBUG) println(s"[${Thread.currentThread().getName}]: release $this, unparking $peeked")
    LockSupport.unpark(peeked)
  }

  override def subsume(subsumableLock: SubsumableLock): SubsumableLock = synchronized {
    assert(subsumableLock != this, s"trying to create endless loops, are you?")
    assert(subsumableLock.getLockedRoot.isDefined, s"subsume partner $subsumableLock is unlocked.")
    assert(subsumableLock.getLockedRoot.get == subsumableLock, s"subsume partner $subsumableLock is subsumed.")
    assert(state.get != null, s"subsume attempt on unlocked $this")
    assert(state.get == Self, s"subsume attempt on subsumed $this")
    if(!state.compareAndSet(Self, subsumableLock)) throw new AssertionError(s"$this subsume failed due to contention!?")
    val peeked = waiters.peek()
    if(DEBUG) println(s"[${Thread.currentThread().getName}]: subsume $this to $subsumableLock, unparking " + (if(peeked == null) "none" else peeked.getName))
    LockSupport.unpark(peeked)
    subsumableLock
  }
}
