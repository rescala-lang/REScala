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

  @tailrec final override def trySubsume0(hopCount: Int, lockedNewParent: SubsumableLock): Future[(Int, Option[SubsumableLock])] = {
    assert(lockedNewParent.host == host, s"trySubsume $this to $lockedNewParent is hosted on ${lockedNewParent.host} different from $host")
    if(lockedNewParent == this) {
      assert(lockedNewParent eq this, s"instance caching broken? $this came into contact with reflection of itself on same host")
      assert(state.get == Self, s"passed in a TryLockResult indicates that $this was successfully locked, but it currently isn't!")
      if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this to itself reentrant success; $hopCount new refs")
      if(hopCount > 0) localAddRefs(hopCount)
      futureZeroNone
    } else {
      state.get match {
        case null =>
          val success = state.compareAndSet(null, lockedNewParent)
          if(success) {
            if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this succeeded; passing ${hopCount + 2} new refs")
            lockedNewParent.localAddRefs(hopCount + 2)
            Future.successful((0, None))
          } else {
            if (DEBUG) println(s"[${Thread.currentThread().getName}]: retrying contended trySubsume $this to $lockedNewParent")
            trySubsume0(hopCount, lockedNewParent)
          }
        case Self =>
          if (DEBUG) println(s"[${Thread.currentThread().getName}]: trySubsume $this to $lockedNewParent blocked; $hopCount new refs")
          if(hopCount > 0) localAddRefs(hopCount)
          Future.successful((0, Some(this)))
        case parent =>
          parent.trySubsume0(hopCount + 1, lockedNewParent).map { res =>
            trySwap(parent, res._2.getOrElse(lockedNewParent), res)
          }(ReactiveTransmittable.notWorthToMoveToTaskpool)
      }
    }
  }

  val waiters = new ConcurrentLinkedQueue[Thread]()
  @tailrec final override def lock0(hopCount: Int): Future[(Int, SubsumableLock)] = {
    state.get match {
      case null =>
        if(state.compareAndSet(null, Self)) {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: locked $this; $hopCount new refs")
          if(hopCount > 0) localAddRefs(hopCount)
          Future.successful((0, this))
        } else {
          if(DEBUG) println(s"[${Thread.currentThread().getName}]: retrying contended lock attempt of $this")
          lock0(hopCount)
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
        lock0(hopCount)
      case parent =>
        LockSupport.unpark(waiters.peek())
        parent.lock0(hopCount + 1).map { res =>
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

  override def asyncUnlock0(): Unit = synchronized {
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

  override def spinOnce0(backoff: Long): Future[(Int, SubsumableLock)] = {
    // this method may seem silly, but serves as an local redirect for preventing back-and-forth remote messages.
    state.get match {
      case null =>
        throw new IllegalStateException(s"spinOnce on unlocked $this")
      case Self =>
        unlock0()
        Backoff.milliSleepNanoSpin(backoff)
        lock0(0)
      case parent =>
        throw new IllegalStateException(s"spinOnce on subsumed $this")
    }
  }

  def trySwap[T](from: SubsumableLock, to: SubsumableLock, res: (Int, T)): (Int, T) = {
    if(from == to) {
      res
    } else if(state.compareAndSet(from, to)) {
      if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this parent cas $from to $to succeeded")
      from.localSubRefs(1)
      res
    } else {
      if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this parent cas $from to $to failed due to contention")
      val (failedRefChanges, t) = res
      (failedRefChanges + 1, t)
    }
  }

  override def toString: String = {
    val refs = refCount.get()
    s"Lock($guid on $host, ${if(refs <= 0) "gc'd" else refs + " refs"}, ${state.get match {
      case null => "unlocked"
      case Self => "locked"
      case other => s"subsumed($other)"
    }})"
  }

  override def remoteAsyncUnlock(): Unit = {
    asyncUnlock0()
  }
  override def remoteLock(): Future[SubsumableLock] = {
    lock0(0).map{ case (failedRefChanges, newRoot) =>
      if(failedRefChanges != 0) {
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning lock result to remote, correcting $failedRefChanges failed ref changes to $newRoot")
        newRoot.localSubRefs(failedRefChanges)
      }
      newRoot
    }(ReactiveTransmittable.notWorthToMoveToTaskpool)
  }
  override def remoteSpinOnce(backoff: Long): Future[SubsumableLock] = {
    spinOnce0(backoff).map{ case (failedRefChanges, newRoot) =>
      if(failedRefChanges != 0) {
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning lock result to remote, correcting $failedRefChanges failed ref changes to $newRoot")
        newRoot.localSubRefs(failedRefChanges)
      }
      newRoot
    }(ReactiveTransmittable.notWorthToMoveToTaskpool)
  }
  override def remoteTrySubsume(lockedNewParent: SubsumableLock): Future[Option[SubsumableLock]] = {
    trySubsume0(0, lockedNewParent).map { case (failedRefChanges, res) =>
      if(failedRefChanges != 0) {
        val newRoot = res.getOrElse(lockedNewParent)
        if(SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}] $this returning trySubsume result to remote, correcting $failedRefChanges failed ref changes to $newRoot")
        newRoot.localSubRefs(failedRefChanges)
      }
      res
    }(ReactiveTransmittable.notWorthToMoveToTaskpool)
  }

  override protected def dumped(): Unit = {
    state.get match {
      case null =>
        if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this no refs remaining, deallocating")
      case Self =>
        throw new AssertionError(s"$this was garbage collected while locked")
      case parent =>
        if (SubsumableLock.DEBUG) println(s"[${Thread.currentThread().getName}]: $this no refs remaining, deallocating and dropping ref on parent $parent")
        parent.localSubRefs(1)
    }
  }
}
