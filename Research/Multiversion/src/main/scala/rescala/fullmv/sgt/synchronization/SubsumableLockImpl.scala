package rescala.fullmv.sgt.synchronization

import java.util.concurrent.{ConcurrentLinkedQueue, ThreadLocalRandom}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.LockSupport

import rescala.fullmv.sgt.synchronization.SubsumableLock._

import scala.annotation.tailrec

class SubsumableLockImpl extends SubsumableLock {
  Self =>
  val DEBUG = false

  val state = new AtomicReference[SubsumableLock](null)

  val gUID: GUID = ThreadLocalRandom.current().nextLong()

  override def getLockedRoot: Option[GUID] = {
    state.get match {
      case null => None
      case Self => Some(gUID)
      case parent => parent.getLockedRoot
    }
  }

  override def tryLock(): TryLockResult = {
    state.get match {
      case null =>
        val success = state.compareAndSet(null, Self)
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: ${if(success) "trylocked" else "failed trylock to contention"} $this")
        TryLockResult(success, this, gUID)
      case Self =>
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: trylock $this blocked")
        TryLockResult(success = false, this, gUID)
      case parent =>
        val res = parent.tryLock()
        state.set(res.newParent)
        res
    }
  }

  val waiters = new ConcurrentLinkedQueue[Thread]()
  @tailrec final override def lock(): TryLockResult = {
    state.get match {
      case null =>
        if(state.compareAndSet(null, Self)) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: locked $this")
          TryLockResult(success = true, this, gUID)
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
        state.set(res.newParent)
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

  override def subsume(opponentLock: TryLockResult): TryLockResult = synchronized {
    assert(opponentLock.success, s"trying to subsume on failed lock")
    assert(opponentLock.globalRoot != gUID, s"trying to create endless loops, are you?")
    assert(opponentLock.newParent.getLockedRoot.isDefined, s"subsume partner $opponentLock is unlocked.")
    assert(state.get != null, s"subsume attempt on unlocked $this")
    assert(state.get == Self, s"subsume attempt on subsumed $this")
    if(!state.compareAndSet(Self, opponentLock.newParent)) throw new AssertionError(s"$this subsume failed due to contention!?")
    val peeked = waiters.peek()
    if(DEBUG) println(s"[${Thread.currentThread().getName}]: subsume $this to $opponentLock, unparking " + (if(peeked == null) "none" else peeked.getName))
    LockSupport.unpark(peeked)
    opponentLock
  }
}
