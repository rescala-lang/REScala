package rescala.fullmv.sgt.synchronization

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.LockSupport

import rescala.fullmv.mirrors.{Host, SubsumableLockHost}
import rescala.fullmv.sgt.synchronization.SubsumableLock._
import rescala.fullmv.transmitter.ReactiveTransmittable
import rescala.parrp.Backoff

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class SubsumableLockImpl(override val host: SubsumableLockHost, override val guid: Host.GUID) extends SubsumableLock {
  Self =>
  val state = new AtomicReference[SubsumableLock](null)

  override def getLockedRoot: Future[Option[Host.GUID]] = {
    state.get match {
      case null => Future.successful(None)
      case Self => Future.successful(Some(guid))
      case parent => parent.getLockedRoot
    }
  }

  override def tryLock(): Future[TryLockResult] = {
    state.get match {
      case null =>
        val success = state.compareAndSet(null, Self)
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: ${if(success) "trylocked" else "failed trylock to contention"} $this")
        Future.successful(TryLockResult(success, this))
      case Self =>
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: trylock $this blocked")
        Future.successful(TryLockResult(success = false, this))
      case parent =>
        val res = parent.tryLock()
        res.foreach { r => state.compareAndSet(parent, r.newParent) }(ReactiveTransmittable.notWorthToMoveToTaskpool)
        res
    }
  }

  override def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = {
    assert(lockedNewParent.host == host, s"trySubsume $this to $lockedNewParent is hosted on ${lockedNewParent.host} different from $host")
    if(lockedNewParent == this) {
      assert(lockedNewParent eq this, s"instance caching broken? $this came into contact with reflection of itself on same host")
      assert(state.get == Self, s"passed in a TryLockResult indicates that $this was successfully locked, but it currently isn't!")
      if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this to itself reentrant success")
      Future.successful(None)
    } else {
      state.get match {
        case null =>
          val success = state.compareAndSet(null, lockedNewParent)
          if(success) {
            if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this succeeded")
            Future.successful(None)
          } else {
            if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this to $lockedNewParent failed to contention")
            Future.successful(Some(this))
          }
        case Self =>
          if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this to $lockedNewParent blocked")
          Future.successful(Some(this))
        case parent =>
          val res = parent.trySubsume(lockedNewParent)
          res.foreach { r => state.compareAndSet(parent, r.getOrElse(lockedNewParent)) }(ReactiveTransmittable.notWorthToMoveToTaskpool)
          res
      }
    }
  }

  val waiters = new ConcurrentLinkedQueue[Thread]()
  @tailrec final override def lock(): Future[SubsumableLock] = {
    state.get match {
      case null =>
        if(state.compareAndSet(null, Self)) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: locked $this")
          Future.successful(this)
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
        val newParent = parent.lock()
        newParent.foreach { p => state.compareAndSet(parent, p) }(ReactiveTransmittable.notWorthToMoveToTaskpool)
        newParent
    }
  }

  override def unlock(): Future[SubsumableLock] = synchronized {
    state.get match {
      case null =>
        throw new IllegalStateException(s"unlock on unlocked $this")
      case Self =>
        if(!state.compareAndSet(Self, null)) throw new AssertionError(s"$this unlock failed due to contention!?")
        val peeked = waiters.peek()
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: release $this, unparking $peeked")
        LockSupport.unpark(peeked)
        Future.successful(this)
      case parent =>
        val newParent = parent.unlock()
        newParent.foreach { p => state.compareAndSet(parent, p)}(ReactiveTransmittable.notWorthToMoveToTaskpool)
        newParent
    }
  }


  override def spinOnce(backoff: Long): Future[SubsumableLock] = {
    // this method may seem silly, but serves as an local redirect for preventing back-and-forth remote messages.
    state.get match {
      case null =>
        throw new IllegalStateException(s"spinOnce on unlocked $this")
      case Self =>
        if(!state.compareAndSet(Self, null)) throw new AssertionError(s"$this unlock failed due to contention!?")
        val peeked = waiters.peek()
        if(DEBUG) println(s"[${Thread.currentThread().getName}]: release $this, unparking $peeked")
        LockSupport.unpark(peeked)
        Backoff.backoff(backoff)
        lock()
      case parent =>
        val newParent = parent.spinOnce(backoff)
        newParent.foreach{ p => state.compareAndSet(parent, p) }(ReactiveTransmittable.notWorthToMoveToTaskpool)
        newParent
    }
  }

  override def subsume(lockedNewParent: SubsumableLock): Future[Unit] = synchronized {
    assert(lockedNewParent.host == host, s"subsume $this to $lockedNewParent is hosted on ${lockedNewParent.host} different from $host")
    assert(lockedNewParent ne this, s"trying to create endless loops, are you?")
    assert(lockedNewParent != this, s"instance caching broken? $this came into contact with reflection of itself on same host")
    assert(Await.result(lockedNewParent.getLockedRoot, Duration.Inf).isDefined, s"subsume partner $lockedNewParent is unlocked.")
    assert(state.get != null, s"subsume attempt on unlocked $this")
    assert(state.get == Self, s"subsume attempt on subsumed $this")
    if(!state.compareAndSet(Self, lockedNewParent)) throw new AssertionError(s"$this subsume failed due to contention!?")
    val peeked = waiters.peek()
    if(DEBUG) println(s"[${Thread.currentThread().getName}]: subsumed $this, unparking " + (if(peeked == null) "none" else peeked.getName))
    LockSupport.unpark(peeked)
    Future.successful(Unit)
  }

  override def toString = s"Lock($guid on $host, ${state.get match {
      case null => "unlocked"
      case Self => "locked"
      case other => s"subsumed($other)"
    }})"
}
