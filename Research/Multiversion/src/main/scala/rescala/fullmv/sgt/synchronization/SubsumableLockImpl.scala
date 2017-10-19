package rescala.fullmv.sgt.synchronization

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.LockSupport

import rescala.fullmv.mirrors.{Host, SubsumableLockHost}
import rescala.fullmv.sgt.synchronization.SubsumableLock._
import rescala.fullmv.transmitter.ReactiveTransmittable
import rescala.parrp.Backoff

import scala.annotation.tailrec
import scala.concurrent.Future

class SubsumableLockImpl(override val host: SubsumableLockHost, override val guid: Host.GUID) extends SubsumableLock {
  Self =>
  val state = new AtomicReference[SubsumableLock](null)

  override def getLockedRoot: Future[Option[Host.GUID]] = {
    state.get match {
      case null => futureNone
      case Self => Future.successful(Some(guid))
      case parent => parent.getLockedRoot
    }
  }

  @tailrec final override def trySubsume(hopCount: Int, lockedNewParent: SubsumableLock): Future[(Int, Option[SubsumableLock])] = {
    assert(lockedNewParent.host == host, s"trySubsume $this to $lockedNewParent is hosted on ${lockedNewParent.host} different from $host")
    if(lockedNewParent == this) {
      assert(lockedNewParent eq this, s"instance caching broken? $this came into contact with reflection of itself on same host")
      assert(state.get == Self, s"passed in a TryLockResult indicates that $this was successfully locked, but it currently isn't!")
      if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this to itself reentrant success")
      addRefs(hopCount)
      futureZeroNone
    } else {
      state.get match {
        case null =>
          val success = state.compareAndSet(null, lockedNewParent)
          if(success) {
            if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this succeeded")
            lockedNewParent.addRefs(hopCount + 2).map(_ => (0, None))(ReactiveTransmittable.notWorthToMoveToTaskpool)
          } else {
            if (DEBUG) println(s"[${Thread.currentThread().getName}]: retrying contended trySubsume $this to $lockedNewParent")
            trySubsume(hopCount, lockedNewParent)
          }
        case Self =>
          if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this to $lockedNewParent blocked")
          addRefs(hopCount)
          Future.successful((0, Some(this)))
        case parent =>
          parent.trySubsume(hopCount + 1, lockedNewParent).map { res =>
            trySwap(parent, res._2.getOrElse(lockedNewParent), res)
          }(ReactiveTransmittable.notWorthToMoveToTaskpool)
      }
    }
  }

  val waiters = new ConcurrentLinkedQueue[Thread]()
  @tailrec final override def lock(hopCount: Int): Future[(Int, SubsumableLock)] = {
    state.get match {
      case null =>
        if(state.compareAndSet(null, Self)) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: locked $this; $hopCount new refs")
          addRefs(hopCount)
          Future.successful((0, this))
        } else {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: retrying contended lock attempt of $this")
          lock(hopCount)
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
        lock(hopCount)
      case parent =>
        LockSupport.unpark(waiters.peek())
        parent.lock(hopCount + 1).map { res =>
          trySwap(parent, res._2, res)
        }(ReactiveTransmittable.notWorthToMoveToTaskpool)
    }
  }

  private def unlock0(): Unit = {
    if (!state.compareAndSet(Self, null)) throw new AssertionError(s"$this unlock failed due to contention!?")
    val peeked = waiters.peek()
    if (DEBUG) println(s"[${Thread.currentThread().getName}]: release $this, unparking $peeked")
    LockSupport.unpark(peeked)
  }

  override def unlock(): Unit = synchronized {
    state.get match {
      case null =>
        throw new IllegalStateException(s"unlock on unlocked $this")
      case Self =>
        unlock0()
        Future.successful(this)
      case parent =>
        throw new IllegalStateException(s"unlock on subsumed $this")
    }
  }

  override def spinOnce(hopCount: Int, backoff: Long): Future[(Int, SubsumableLock)] = {
    // this method may seem silly, but serves as an local redirect for preventing back-and-forth remote messages.
    state.get match {
      case null =>
        throw new IllegalStateException(s"spinOnce on unlocked $this")
      case Self =>
        unlock0()
        Backoff.milliSleepNanoSpin(backoff)
        lock(hopCount)
      case parent =>
        parent.spinOnce(hopCount + 1, backoff).map { res =>
          trySwap(parent, res._2, res)
        }(ReactiveTransmittable.notWorthToMoveToTaskpool)
    }
  }

  def trySwap[T](from: SubsumableLock, to: SubsumableLock, res: (Int, T)): (Int, T) = {
    if(from == to) {
      res
    } else if(state.compareAndSet(from, to)) {
      from.asyncSubRefs(1)
      res
    } else {
      if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this parent cas $from -> $to preempted by contention")
      val (failedRefChanges, t) = res
      (failedRefChanges + 1, t)
    }
  }

  override def toString = {
    val refs = refCount.get()
    s"Lock($guid on $host, ${state.get match {
      case null => "unlocked"
      case Self => "locked"
      case other => s"subsumed($other)"
    }}, ${if(refs <= 0) "gc'd" else refs + " refs"})"
  }

  override def lock(): Future[SubsumableLock] = {
    lock(0).map{ case (failedRefChanges, newRoot) =>
      if(failedRefChanges != 0) newRoot.asyncSubRefs(failedRefChanges)
      newRoot
    }(ReactiveTransmittable.notWorthToMoveToTaskpool)
  }
  override def spinOnce(backoff: Long): Future[SubsumableLock] = {
    spinOnce(0, backoff).map{ case (failedRefChanges, newRoot) =>
      if(failedRefChanges != 0) newRoot.asyncSubRefs(failedRefChanges)
      newRoot
    }(ReactiveTransmittable.notWorthToMoveToTaskpool)
  }
  override def trySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = {
    trySubsume(0, lockedNewParent).map { case (failedRefChanges, res) =>
      if(failedRefChanges != 0) res.getOrElse(lockedNewParent).asyncSubRefs(failedRefChanges)
      res
    }(ReactiveTransmittable.notWorthToMoveToTaskpool)
  }

  override protected def dumped(): Unit = {
    state.get match {
      case null => // do nothing
      case Self => throw new AssertionError(s"$this was garbage collected while locked")
      case parent => parent.remoteRefDropped()
    }
  }
}
