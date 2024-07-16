package reactives.fullmv

import reactives.core.Derived
import reactives.structure.Pulse

import java.util.concurrent.ForkJoinPool
import java.util.concurrent.ForkJoinPool.ManagedBlocker
import java.util.concurrent.atomic.{AtomicIntegerFieldUpdater, AtomicReference, AtomicReferenceFieldUpdater}
import java.util.concurrent.locks.LockSupport
import scala.annotation.{elidable, nowarn, tailrec}

import scala.language.unsafeNulls

sealed trait MaybeWritten[+V]
case object NotFinal                                      extends MaybeWritten[Nothing]
case object Unwritten                                     extends MaybeWritten[Nothing]
case class Written[V](valueForSelf: V, valueForFuture: V) extends MaybeWritten[V]

/** A node version history datastructure
  *
  * @param init             the initial creating transaction
  * @param valuePersistency the value persistency descriptor
  * @tparam V      the type of stored values
  * @tparam T      the type of transactions
  * @tparam InDep  the type of incoming dependency nodes
  * @tparam OutDep the type of outgoing dependency nodes
  */
class NonblockingSkipListVersionHistory[V, T <: FullMVTurn](init: T, val valuePersistency: V)
    extends FullMVState[V, T] {
  override val host = init.host

  /** @param txn                  the transaction to which this version belongs
    * @param previousWriteIfStable if set, this version is guaranteed to be stable. if null, stable must be verified by comparing firstFrame and possibly traversing the history.
    * @param value                 current state of this version
    * @param next                  the successor version; the chain of next links forms the ground truth for which versions are contained in which order in the node's history.
    */
  final class QueuedVersion(
      val txn: T,
      @volatile var previousWriteIfStable: QueuedVersion,
      @volatile var value: MaybeWritten[V],
      next: QueuedVersion
  ) extends LinkWithCounters[QueuedVersion](next) {
    def incrementPending(): Int =
      NonblockingSkipListVersionHistory.pendingUpdate.incrementAndGet(this.asInstanceOf[LinkWithCounters[Any]])
    def decrementPending(): Int =
      NonblockingSkipListVersionHistory.pendingUpdate.decrementAndGet(this.asInstanceOf[LinkWithCounters[Any]])
    def incrementChanged(): Int =
      NonblockingSkipListVersionHistory.changedUpdate.incrementAndGet(this.asInstanceOf[LinkWithCounters[Any]])
    def decrementChanged(): Int =
      NonblockingSkipListVersionHistory.changedUpdate.decrementAndGet(this.asInstanceOf[LinkWithCounters[Any]])
    @tailrec def addSleeper(atomicReferenceFieldUpdater: AtomicReferenceFieldUpdater[
      LinkWithCounters[Any],
      List[Thread]
    ]): Boolean = {
      val before = atomicReferenceFieldUpdater.get(this.asInstanceOf[LinkWithCounters[Any]])
      if before == null then {
        false
      } else if
        atomicReferenceFieldUpdater.compareAndSet(
          this.asInstanceOf[LinkWithCounters[Any]],
          before,
          Thread.currentThread() :: before
        )
      then {
        true
      } else {
        addSleeper(atomicReferenceFieldUpdater)
      }
    }

    @tailrec def wakeAll(threads: List[Thread], reason: String): Unit = {
      if threads != null && threads != Nil then {
        if NonblockingSkipListVersionHistory.DEBUG || FullMVUtil.DEBUG then
          println(s"[${Thread.currentThread().getName}] $reason $this, unparking ${threads.head}.")
        LockSupport.unpark(threads.head)
        wakeAll(threads.tail, reason)
      }
    }

    def ensureStabilized(write: QueuedVersion): Unit = {
      assert(write.isWritten, s"may not stabilize to unwritten $write")
      assert(write != this, s"may not self-stabilize $write")
      if previousWriteIfStable == null then {
        assert(value == NotFinal || previousWriteIfStable != null, s"value set despite not even stable on $this")
        previousWriteIfStable = write

        val unpark = stableSleepers
        NonblockingSkipListVersionHistory.stableSleeperUpdate.lazySet(this.asInstanceOf[LinkWithCounters[Any]], null)
        wakeAll(unpark, "stabilized")

        if isZeroCounters then finalize(Unwritten)
      } else {
        assert(
          write == previousWriteIfStable,
          s"attempt to stabilize $this to different $write (last written predecessor *should* be unambigous)"
        )
        if !isFinal && isZeroCounters then finalize(Unwritten)
      }
    }

    def finalize(v: MaybeWritten[V]): Unit = {
      assert(value == NotFinal || (value == Unwritten && v == Unwritten), s"may only write to non-final $this")
      value = v
      finalized()
    }

    private def finalized(): Unit = {
      val unpark = finalSleepers
      NonblockingSkipListVersionHistory.finalSleeperUpdate.lazySet(this.asInstanceOf[LinkWithCounters[Any]], null)
      wakeAll(unpark, "finalized")
    }

    def isZeroCounters: Boolean = pending == 0 && changed == 0
    def isStable: Boolean       = previousWriteIfStable != null
    def isFinal: Boolean =
      value match {
        case Unwritten     => true
        case Written(_, _) => true
        case _             => false
      }
    def isWritten: Boolean =
      value match {
        case Written(_, _) => true
        case _             => false
      }
    def readForSelf: V =
      value match {
        case Written(valueForSelf, _) => valueForSelf
        case _                        => throw new NoSuchElementException(this.toString + " is not written!")
      }
    def readForFuture: V =
      value match {
        case Written(_, valueForFuture) => valueForFuture
        case _                          => throw new NoSuchElementException(this.toString + " is not written!")
      }

    override def toString: String = {
      s"Version($hashCode of $txn, p=$pending, c=$changed, v=$value, stable=$previousWriteIfStable)"
    }
  }

  val laggingLatestStable = new AtomicReference(new QueuedVersion(
    init,
    null, {
      val iv = valuePersistency
      // TODO: this SHOULD be Written(iv, unchange(iv)), but unchange is no longer available at this point …
      Written(iv, iv)
    },
    null
  ))
  // synchronized, written sequentially only if firstFrame.txn.phase == Executing && queueHead == firstFrame by notify/reevOut
  @volatile var latestValue: V = laggingLatestStable.get.readForFuture

  // =================== STORAGE ====================

  /** pointer that reflects, which firstFrame is currently communicated to all successor nodes.
    * writes are sequentialized and synchronized with outgoings changes through the object monitor (this.synchronized)
    */
  @volatile var firstFrame: QueuedVersion = null

  /** a pointer to some random version at or behind latestStable, used for periodic O(1) gc support
    * accesses are executed only inside reevOut, and are therefore sequential by nature.
    */
  var lazyPeriodicGC: QueuedVersion = laggingLatestStable.get

  /** successor nodes that are currently sbuscribed for cange notifications.
    * accesses are sequentialized and synchronized with firstFrame changes through the object monitor (this.synchronized)
    */
  var outgoings: Set[OutDep] = Set.empty

  protected def casLatestStable(from: QueuedVersion, to: QueuedVersion): Unit = {
    assert(to.isWritten || to.isStable, s"new latestStable not stable: $to")
    if from != to && laggingLatestStable.compareAndSet(from, to) then {
      val ff = firstFrame
      if ff == null || (ff != from && ff.txn.isTransitivePredecessor(from.txn)) then from.lazySet(from)
    }
  }

  // =================== FRAMING ====================

  @tailrec private def enqueueFraming(txn: T, current: QueuedVersion): QueuedVersion = {
    val next = current.get
    if next == current then {
      if NonblockingSkipListVersionHistory.DEBUG then
        println(s"[${Thread.currentThread().getName}] enqueueFraming fell of the list on $next.")
      null
    } else if next != null && next.txn == txn then {
      if NonblockingSkipListVersionHistory.DEBUG then
        println(s"[${Thread.currentThread().getName}] enqueueFraming found target $next.")
      next
    } else if next == null || next.txn.isTransitivePredecessor(txn) then {
      if NonblockingSkipListVersionHistory.tryFixPredecessorOrderIfNotFixedYet(current.txn, txn) then {
        val v = tryInsertVersion(txn, current, next, null, NotFinal)
        if v != null then {
          if NonblockingSkipListVersionHistory.DEBUG then
            println(s"[${Thread.currentThread().getName}] enqueueFraming created $v between $current and $next.")
          v
        } else {
          enqueueFraming(txn, current)
        }
      } else {
        if NonblockingSkipListVersionHistory.DEBUG then
          println(s"[${Thread.currentThread().getName}] enqueueFraming failed speculative ordering $current < $txn.")
        null
      }
    } else {
      enqueueFraming(txn, next)
    }
  }

  /** entry point for regular framing
    *
    * @param txn the transaction visiting the node for framing
    */
  def incrementFrame(txn: T): FramingBranchResult[T, OutDep] = {
    var version: QueuedVersion = null
    while version == null do version = enqueueFraming(txn, laggingLatestStable.get)
    val result = synchronized { incrementFrame0(version) }
    if NonblockingSkipListVersionHistory.DEBUG then
      println(s"[${Thread.currentThread().getName}] incrementFrame $txn => $result.")
    result
  }

  /** entry point for superseding framing
    *
    * @param txn       the transaction visiting the node for framing
    * @param supersede the transaction whose frame was superseded by the visiting transaction at the previous node
    */
  def incrementSupersedeFrame(txn: T, supersede: T): FramingBranchResult[T, OutDep] = {
    var version: QueuedVersion = null
    while version == null do version = enqueueFraming(txn, laggingLatestStable.get)
    var supersedeVersion: QueuedVersion = null
    while supersedeVersion == null do supersedeVersion = enqueueFraming(supersede, version)
    val result = synchronized {
      supersedeVersion.decrementPending()
      incrementFrame0(version)
    }
    if NonblockingSkipListVersionHistory.DEBUG then
      println(s"[${Thread.currentThread().getName}] incrementFrame $txn, $supersede => $result.")
    result
  }

  private def incrementFrame0(version: QueuedVersion): FramingBranchResult[T, OutDep] = {
    version.incrementPending()
    val ff = firstFrame
    if version == ff then {
      assert(version.pending != 1, s"previously not a frame $version was already pointed to as firstFrame in $this")
      if version.pending == 0 then {
        // if first frame was removed (i.e., overtake compensation was resolved -- these cases mirror progressToNextWriteForNotification)
        var newFirstFrame = version.get()
        while newFirstFrame != null && newFirstFrame.pending == 0 do newFirstFrame = newFirstFrame.get()
        firstFrame = newFirstFrame
        if newFirstFrame == null || newFirstFrame.pending < 0 then {
          FramingBranchResult.FramingBranchEnd
        } else {
          FramingBranchResult.Frame(outgoings, newFirstFrame.txn)
        }
      } else {
        // just incremented an already existing and propagated frame
        FramingBranchResult.FramingBranchEnd
      }
    } else if ff == null || lessThanAssumingNoRaceConditions(version.txn, ff.txn) then {
      // created a new frame
      assert(version.pending == 1, s"found overtake or frame compensation $version before firstFrame in $this")
      firstFrame = version
      if ff == null || ff.pending < 0 then {
        FramingBranchResult.Frame(outgoings, version.txn)
      } else {
        FramingBranchResult.FrameSupersede(outgoings, version.txn, ff.txn)
      }
    } else {
      // created or incremented a non-first frame
      FramingBranchResult.FramingBranchEnd
    }
  }

  /*
   * =================== NOTIFICATIONS/ / REEVALUATION ====================
   */

  @tailrec private def enqueueNotifying(txn: T, current: QueuedVersion): QueuedVersion = {
    val next = current.get
    if next == current then {
      if NonblockingSkipListVersionHistory.DEBUG then
        println(s"[${Thread.currentThread().getName}] enqueueExecuting fell of the list on $next.")
      null
    } else if next != null && next.txn == txn then {
      if NonblockingSkipListVersionHistory.DEBUG then
        println(s"[${Thread.currentThread().getName}] enqueueExecuting found target $next.")
      next
    } else if next == null || NonblockingSkipListVersionHistory.tryFixSuccessorOrderIfNotFixedYet(txn, next.txn) then {
      if NonblockingSkipListVersionHistory.tryFixPredecessorOrderIfNotFixedYet(current.txn, txn) then {
        val v = tryInsertVersion(txn, current, next, null, NotFinal)
        if v != null then {
          if NonblockingSkipListVersionHistory.DEBUG then
            println(s"[${Thread.currentThread().getName}] enqueueExecuting created $v between $current and $next.")
          v
        } else {
          enqueueNotifying(txn, current)
        }
      } else {
        if NonblockingSkipListVersionHistory.DEBUG then
          println(
            s"[${Thread.currentThread().getName}] enqueueExecuting failed speculative ordering $current < $txn."
          )
        null
      }
    } else {
      enqueueNotifying(txn, next)
    }
  }

  @tailrec private def enqueueNotifying(txn: T): QueuedVersion = {
    val ff = firstFrame
    if ff.txn == txn then {
      if NonblockingSkipListVersionHistory.DEBUG then
        println(s"[${Thread.currentThread().getName}] notify on firstFrame $ff.")
      ff
    } else {
      // if we are not firstFrame, enqueue an unstable version
      val version = enqueueNotifying(txn, ff)
      if version != null then {
        version
      } else {
        enqueueNotifying(txn)
      }
    }
  }

  @nowarn()
  @tailrec private def appendAllReachable(builder: StringBuilder, current: QueuedVersion): StringBuilder = {
    builder.append(current).append("\r\n")
    val next = current.get()
    if next == null then {
      builder.append("<end>")
    } else if next == current then {
      builder.append("<dropped>")
    } else {
      appendAllReachable(builder, next)
    }
  }

  @tailrec private def ensureStableOnInsertedExecuting(
      current: QueuedVersion,
      trackedStable: QueuedVersion,
      self: QueuedVersion,
      assertSelfMayBeDropped: Boolean
  ): QueuedVersion = {
    assert(
      assertSelfMayBeDropped || current != null,
      s"$self was supposed to not become final, but everthing is final because there are no more frames."
    )
    assert(
      assertSelfMayBeDropped || current == self || self.txn.isTransitivePredecessor(current.txn),
      s"$self was supposed to not become final, but fell before firstFrame $current"
    )
    if current == null || current == self || lessThanAssumingNoRaceConditions(self.txn, current.txn) then {
      val stable =
        if trackedStable != null then {
          trackedStable
        } else {
          var stable = laggingLatestStable.get()
          if !stable.isWritten then stable = stable.previousWriteIfStable
          while stable.txn == self.txn || lessThanAssumingNoRaceConditions(self.txn, stable.txn) do
            stable = stable.previousWriteIfStable
          stable
        }
      self.ensureStabilized(stable)
      stable
    } else {
      if current.isZeroCounters then {
        val next = current.get()
        if next == current then {
          ensureStableOnInsertedExecuting(firstFrame, null, self, assertSelfMayBeDropped)
        } else {
          val nextStable =
            if current.isWritten then {
              current
            } else if trackedStable != null then {
              // could current.stabilize(trackedStable) here?
              trackedStable
            } else {
              current.previousWriteIfStable
            }
          ensureStableOnInsertedExecuting(next, nextStable, self, assertSelfMayBeDropped)
        }
      } else {
        // current is frame => self is not stable
        null
      }
    }
  }

  /** entry point for change/nochange notification reception
    *
    * @param txn     the transaction sending the notification
    * @param changed whether or not the dependency changed
    */
  def notify(txn: T, changed: Boolean): (Boolean, NotificationBranchResult[T, OutDep]) = {
    val version = enqueueNotifying(txn)
    val result  = notify0(version, changed)
    if NonblockingSkipListVersionHistory.DEBUG then
      println(s"[${Thread.currentThread().getName}] notify $txn with change=$changed => $result.")
    result
  }

  private def enqueueFollowFraming(txn: T, current: QueuedVersion): QueuedVersion = {
    if txn.phase == TurnPhase.Executing then {
      if NonblockingSkipListVersionHistory.DEBUG then
        println(
          s"[${Thread.currentThread().getName}] enqueueFollowFraming delegating to enqueueNotifying for executing $txn."
        )
      enqueueNotifying(txn, current)
    } else {
      enqueueFollowFramingFraming(txn, current)
    }
  }

  @tailrec private def enqueueFollowFramingFraming(txn: T, current: QueuedVersion): QueuedVersion = {
    val next = current.get()
    assert(
      next != current,
      s"notifying found $next fell off the list, which should be impossible because notify happens behind firstFrame, but only before firstFrame can fall of the list"
    )
    if next != null && next.txn == txn then {
      if NonblockingSkipListVersionHistory.DEBUG then
        println(s"[${Thread.currentThread().getName}] enqueueFollowFraming found target $next.")
      next
    } else if
      next == null || next.txn.isTransitivePredecessor(
        txn
      ) /* assuming we are still Framing, always go as far back as possible */
    then {
      if
        txn.isTransitivePredecessor(current.txn) || (current.txn.phase match {
          case TurnPhase.Completed => true
          case TurnPhase.Executing =>
            NonblockingSkipListVersionHistory.tryRecordRelationship(current.txn, txn, current.txn, txn)
          case TurnPhase.Framing => /* and only if still Framing becomes relevant, ensure that we still are */
            FullMVUtil.myAwait(
              txn.acquireRemoteBranchIfPhaseAtMost(TurnPhase.Framing),
              txn.host.timeout
            ) <= TurnPhase.Framing && (try {
              NonblockingSkipListVersionHistory.tryRecordRelationship(current.txn, txn, current.txn, txn)
            } finally {
              txn.asyncRemoteBranchComplete(TurnPhase.Framing)
            })
          case other => throw new IllegalStateException(s"$other is not a phase")
        })
      then {
        val v = tryInsertVersion(txn, current, next, null, NotFinal)
        if v != null then {
          if NonblockingSkipListVersionHistory.DEBUG then
            println(
              s"[${Thread.currentThread().getName}] enqueueFollowFraming created $v between $current and $next."
            )
          v
        } else {
          enqueueFollowFramingFraming(txn, current)
        }
      } else {
        if NonblockingSkipListVersionHistory.DEBUG then
          println(
            s"[${Thread.currentThread().getName}] enqueueFollowFraming failed speculative ordering $current < $txn."
          )
        null
      }
    } else {
      enqueueFollowFramingFraming(txn, next)
    }
  }

  /** entry point for change/nochange notification reception with follow-up framing
    *
    * @param txn         the transaction sending the notification
    * @param changed     whether or not the dependency changed
    * @param followFrame a transaction for which to create a subsequent frame, furthering its partial framing.
    */
  def notifyFollowFrame(txn: T, changed: Boolean, followFrame: T): (Boolean, NotificationBranchResult[T, OutDep]) = {
    val version: QueuedVersion       = enqueueNotifying(txn)
    var followVersion: QueuedVersion = null
    while followVersion == null do followVersion = enqueueFollowFraming(followFrame, version)
    followVersion.incrementPending()
    val result = notify0(version, changed)
    if NonblockingSkipListVersionHistory.DEBUG then
      println(
        s"[${Thread.currentThread().getName}] notify $txn with change=$changed and followFrame $followFrame => $result."
      )
    result
  }

  private def notify0(version: QueuedVersion, changed: Boolean): (Boolean, NotificationBranchResult[T, OutDep]) =
    synchronized {
      if version == firstFrame then {
        val ls = laggingLatestStable.get()
        if ls != version then {
          version.ensureStabilized(if ls.isWritten then ls else ls.previousWriteIfStable)
          laggingLatestStable.set(version)
          ls.lazySet(ls)
        }
      }

      // note: if drop retrofitting overtook the change notification, change may update from -1 to 0 here!
      val retainBranch = changed && version.incrementChanged() == 1

      // note: if the notification overtook a previous turn's notification with followFraming for this transaction,
      // or drop retrofitting with firstFrame retrofit for followFraming, then pending may update from 0 to -1 here
      version.decrementPending()

      // if(java.util.concurrent.ThreadLocalRandom.current().nextDouble < .05d) Thread.`yield`()
      // check if the notification triggers subsequent actions
      if version == firstFrame && version.pending == 0 then {
        if version.changed > 0 then {
          (retainBranch, NotificationBranchResult.ReevaluationReady)
        } else {
          // ResolvedFirstFrameToUnchanged
          assert(
            !retainBranch,
            s"On second thought, I think this *can* happen, because drops are non-blocking and may drop the 0->1 change back to 0 between above and here, but it hasn't yet, so let's see if it actually fails at some point."
          )
          version.value = Unwritten
          (true, findNextAndUpdateFirstFrame(version, version, version.previousWriteIfStable))
        }
      } else {
        (retainBranch, NotificationBranchResult.DoNothing)
      }
    }

  def reevIn(txn: T): V = {
    assert(
      laggingLatestStable.get == firstFrame,
      s"reevIn by $txn: latestStable ${laggingLatestStable.get} != firstFrame $firstFrame"
    )
    assert(firstFrame.txn == txn, s"$txn called reevIn, but firstFrame is $firstFrame")
    assert(firstFrame.pending == 0, s"still pending notifications: $firstFrame")
    assert(firstFrame.changed > 0, s"should not reevaluate without change: $firstFrame")
    latestValue
  }

  def reevOut(
      turn: T,
      maybeValue: Option[V],
      unchange: V => V
  ): NotificationBranchResult.ReevOutBranchResult[T, OutDep] = {
    val version = firstFrame
    assert(
      laggingLatestStable.get == version,
      s"reevOut by $turn: latestStable ${laggingLatestStable.get} != firstFrame $version"
    )
    assert(version.txn == turn, s"$turn called reevDone, but first frame is $version (different transaction)")
    assert(version.pending >= 0, s"firstFrame with pending < 0 should be impossible")
    assert(version.changed >= 0, s"firstFrame with changed < 0 should be impossible")
    assert(
      !version.isZeroCounters || maybeValue.isEmpty,
      s"$turn cannot write change to ${maybeValue.get} into non-frame $version"
    )
    assert(!version.isFinal, s"$turn cannot write twice: $version")

    assert(
      version.pending == 0,
      s"$turn completed reevaluation, but first frame $version is no longer glitch free ready -- retrofitting during reevaluation could cause this, but dynamicAfter should be implemented to suspend for final, which should make such a retrofitting case impossible."
    )
    assert(
      version.changed > 0 || (version.changed == 0 && maybeValue.isEmpty),
      s"$turn cannot write changed=${maybeValue.isDefined} in $this"
    )
    if NonblockingSkipListVersionHistory.DEBUG || FullMVUtil.DEBUG then
      maybeValue match {
        case Some(Pulse.Exceptional(t)) =>
          throw new Exception(
            s"Glitch-free reevaluation result for $version is exceptional",
            maybeValue.get.asInstanceOf[Pulse.Exceptional].throwable
          )
        case _ => // ignore
      }
    synchronized {
      val stabilizeTo =
        if maybeValue.isDefined then {
          val selfValue   = maybeValue.get
          val futureValue = unchange(selfValue)
          version.finalize(Written(selfValue, futureValue))
          if lazyPeriodicGC.txn.phase == TurnPhase.Completed then {
            lazyPeriodicGC.previousWriteIfStable = null
            lazyPeriodicGC = version
          }
          latestValue = futureValue
          version
        } else {
          version.finalize(Unwritten)
          version.previousWriteIfStable
        }
      version.changed = 0 // no need to use the atomic updater here, because no racing threads can exist anymore.
      val res = findNextAndUpdateFirstFrame(version, version, stabilizeTo)
      if NonblockingSkipListVersionHistory.TRACE_VALUES then {
        if maybeValue.isDefined then {
          println(s"[${Thread.currentThread().getName}] reevOut $turn wrote $version => $res")
        } else {
          println(s"[${Thread.currentThread().getName}] reevOut $turn left $version unchanged => $res.")
        }
      }
      res
    }
  }

  @tailrec private def findNextAndUpdateFirstFrame(
      finalizedVersion: QueuedVersion,
      current: QueuedVersion,
      stabilizeTo: QueuedVersion
  ): NotificationBranchResult.ReevOutBranchResult[T, OutDep] = {
    assert(current.isStable, s"pushStable from $current must be final, which implies stable, but not set stable.")
    assert(current.pending == 0, s"pushStable from $current must be final (pending)")
    assert(current.changed == 0, s"pushStable from $current must be final (changed)")
    val next = current.get()
    assert(next != current, s"someone pushed pushStable after finalizing $finalizedVersion off the list at $next")
    if next != null && next.txn.phase == TurnPhase.Executing then {
      // next is stable ...
      next.ensureStabilized(stabilizeTo)
      if next.isZeroCounters then {
        // ... and final
        findNextAndUpdateFirstFrame(finalizedVersion, next, stabilizeTo)
      } else {
        // ... but not final (i.e. next is frame, and thus new firstFrame)
        firstFrame = next
        val from = laggingLatestStable.get()
        if from != next then {
          laggingLatestStable.set(next)
          from.lazySet(from)
        }
        finalizedVersion.lazySet(finalizedVersion)
        decideSuccessorOperation(next)
      }
    } else {
      // next is not stable
      // look for an unstable firstFrame
      var newFirstFrame = next
      while newFirstFrame != null && newFirstFrame.isZeroCounters do {
        newFirstFrame = newFirstFrame.get()
      }
      firstFrame = newFirstFrame
      casLatestStable(finalizedVersion, current)
      decideSuccessorOperation(newFirstFrame)
    }
  }

  private def decideSuccessorOperation(newFirstFrame: QueuedVersion)
      : NotificationBranchResult.ReevOutBranchResult[T, OutDep] = {
    if newFirstFrame != null then {
      if newFirstFrame.pending == 0 then {
        NotificationBranchResult.ReevOutBranchResult.NotifyAndReevaluationReadySuccessor(outgoings, newFirstFrame.txn)
      } else if newFirstFrame.pending > 0 then {
        NotificationBranchResult.ReevOutBranchResult.NotifyAndNonReadySuccessor(outgoings, newFirstFrame.txn)
      } else
        /* if(maybeNewFirstFrame.pending < 0) */ {
          NotificationBranchResult.ReevOutBranchResult.PureNotifyOnly(outgoings)
        }
    } else {
      NotificationBranchResult.ReevOutBranchResult.PureNotifyOnly(outgoings)
    }
  }

  // =================== READ OPERATIONS ====================

  /** check whether txn < succ, knowing that either txn < succ or succ < txn must be the case, unless the given planned
    * CAS comparison becomes invalid.
    *
    * @param txn             is expected to be executing
    * @param succ
    * @param casReference    the atomic refenence to be CAS'd
    * @param casVerify       the CAS comparison value
    * @param casInvalidValue the value to return if the CAS condition is no longer valid
    * @return true if txn < succ, false if succ < txn (or succ completed), may stall briefly if racing order establishment
    */
  @tailrec private def lessThanRaceConditionSafe(
      txn: T,
      succ: QueuedVersion,
      casReference: QueuedVersion,
      casVerify: QueuedVersion,
      casInvalidValue: Boolean
  ): Boolean = {
    if succ.txn.phase == TurnPhase.Framing || succ.txn.isTransitivePredecessor(txn) then {
      true
    } else if succ.txn.phase == TurnPhase.Completed || txn.isTransitivePredecessor(succ.txn) then {
      false
    } else {
      Thread.`yield`()
      if casReference.get eq casVerify then {
        lessThanRaceConditionSafe(txn, succ, casReference, casVerify, casInvalidValue)
      } else {
        casInvalidValue
      }
    }
  }

  private def lessThanAssumingNoRaceConditions(txn: T, succ: T): Boolean = {
    val res = succ.isTransitivePredecessor(txn)
    //    assert(res || txn.isTransitivePredecessor(succ) || succ.phase == TurnPhase.Completed, s"apparently this check is NOT safe against racing order establishments and must be replaced by a looped check\r\n\tsearching for: $txn\r\n\tfound unordered: $succ\r\n\treadTracker: ${perThreadReadTracker.get()()}\r\nstate: $this")
    assert(
      res || txn.isTransitivePredecessor(succ) || succ.phase == TurnPhase.Completed,
      s"apparently this check is NOT safe against racing order establishments and must be replaced by a looped check\r\n\tsearching for: $txn\r\n\tfound unordered: $succ\r\n\treadTracker disabled..\r\nstate: $this"
    )
    res
  }

  //  val perThreadReadTracker = new ThreadLocal[() => String] {
  //    override def initialValue(): () => String = () => "was never set"
  //  }

  @tailrec private def enqueueReading(
      txn: T,
      ls: QueuedVersion,
      current: QueuedVersion,
      assertInsertAllowed: Boolean
  ): QueuedVersion = {
    val next = current.get
    if next == current then {
      if NonblockingSkipListVersionHistory.DEBUG then
        println(s"[${Thread.currentThread().getName}] enqueueReading fell of the list on $next.")
      null
    } else if next != null && next.txn == txn then {
      if !next.isStable then
        ensureStableOnInsertedExecuting(firstFrame, null, next, assertSelfMayBeDropped = true)
        ()
      next
      //      if(!next.isStable) {
      //        if (ensureStableOnInsertedExecuting(firstFrame, null, next, assertSelfMayBeDropped = true) != null) {
      //          if (NonblockingSkipListVersionHistory.DEBUG) println(s"[${Thread.currentThread().getName}] enqueueReading found and stabilized target $next.")
      //          next -> "found-stabilized"
      //        } else {
      //          if (NonblockingSkipListVersionHistory.DEBUG) println(s"[${Thread.currentThread().getName}] enqueueReading found unstable target $next.")
      //          next -> "found-unstable"
      //        }
      //      } else {
      //        if (NonblockingSkipListVersionHistory.DEBUG) println(s"[${Thread.currentThread().getName}] enqueueReading found stable target $next.")
      //        next -> "found-stable"
      //      }
    } else if next == null || NonblockingSkipListVersionHistory.tryFixSuccessorOrderIfNotFixedYet(txn, next.txn) then {
      //      assert(assertInsertAllowed, s"could not find assumed existing version of $txn, read tracker says ${perThreadReadTracker.get()()}")
      assert(assertInsertAllowed, s"could not find assumed existing version of $txn, read tracker disabled")
      if NonblockingSkipListVersionHistory.tryFixPredecessorOrderIfNotFixedYet(current.txn, txn) then {
        val ff = firstFrame
        if ff != null && ff.txn == txn then {
          if NonblockingSkipListVersionHistory.DEBUG then
            println(
              s"[${Thread.currentThread().getName}] enqueueReading computing stable for insert was raced target having been created as firstFrame $ff."
            )
          ff // -> "raced ff"
        } else {
          val stable =
            if ff == null || lessThanRaceConditionSafe(txn, ff, current, next, casInvalidValue = false) then {
              assert(
                ff == null || ff.txn.isTransitivePredecessor(txn) || current.get() != next,
                s"this is what above checks *should* ensure.."
              )
              var stable = laggingLatestStable.get()
              if !stable.isWritten then stable = stable.previousWriteIfStable
              while
                stable.txn == txn || lessThanRaceConditionSafe(txn, stable, current, next, casInvalidValue = false)
              do
                stable = stable.previousWriteIfStable
              stable
            } else {
              null
            }
          val v = tryInsertVersion(txn, current, next, stable, if stable == null then NotFinal else Unwritten)
          if v != null then {
            if stable == null then {
              if ff.isZeroCounters then
                ensureStableOnInsertedExecuting(firstFrame, null, v, assertSelfMayBeDropped = true)
                ()
            } else {
              casLatestStable(ls, v)
            }
            v
            //            if (stable == null) {
            //              if (ff.isZeroCounters) {
            //                if (ensureStableOnInsertedExecuting(firstFrame, null, v, assertSelfMayBeDropped = true) != null) {
            //                  if (NonblockingSkipListVersionHistory.DEBUG) println(s"[${Thread.currentThread().getName}] enqueueReading created and re-stabilized $v between $current and $next.")
            //                  v -> "insert-restabilize"
            //                } else {
            //                  if (NonblockingSkipListVersionHistory.DEBUG) println(s"[${Thread.currentThread().getName}] enqueueReading created re-unstable $v between $current and $next.")
            //                  v -> "insert-reunstable"
            //                }
            //              } else {
            //                if (NonblockingSkipListVersionHistory.DEBUG) println(s"[${Thread.currentThread().getName}] enqueueReading created unstable $v between $current and $next.")
            //                v -> "insert-unstable"
            //              }
            //            } else {
            //              if (NonblockingSkipListVersionHistory.DEBUG) println(s"[${Thread.currentThread().getName}] enqueueReading created stable $v between $current and $next.")
            //              v -> "insert-stable"
            //            }
          } else {
            enqueueReading(txn, ls, current, assertInsertAllowed)
          }
        }
      } else {
        if NonblockingSkipListVersionHistory.DEBUG then
          println(s"[${Thread.currentThread().getName}] enqueueReading failed speculative ordering $current < $txn.")
        null
      }
    } else {
      enqueueReading(txn, ls, next, assertInsertAllowed)
    }
  }

  @tailrec private def ensureDynamicReadVersion(
      txn: T,
      assertSleepingAllowed: Boolean = true,
      assertInsertAllowed: Boolean = true
  ): QueuedVersion = {
    val ls = laggingLatestStable.get()
    if ls.txn == txn then {
      ls
    } else if ls.txn.isTransitivePredecessor(txn) then {
      var ownOrPredecessor = ls
      // start at latest write
      if !ownOrPredecessor.isWritten then ownOrPredecessor = ownOrPredecessor.previousWriteIfStable
      // hop back in time over all writes that are considered in the future of txn
      while
        ownOrPredecessor.txn != txn && (ownOrPredecessor.txn.isTransitivePredecessor(
          txn
        ) || !NonblockingSkipListVersionHistory.tryFixPredecessorOrderIfNotFixedYet(ownOrPredecessor.txn, txn))
      do
        ownOrPredecessor = ownOrPredecessor.previousWriteIfStable
      ownOrPredecessor
    } else {
      val versionInQueue = enqueueReading(txn, ls, ls, assertInsertAllowed)
      if versionInQueue == null then {
        ensureDynamicReadVersion(txn, assertSleepingAllowed, assertInsertAllowed)
      } else {
        versionInQueue
      }
    }
  }

  /** entry point for before(this); may suspend.
    *
    * @param txn the executing transaction
    * @return the corresponding value from before this transaction, i.e., ignoring the transaction's
    *         own writes.
    */
  def dynamicBefore(txn: T): V =
    try {
      val selfOrFinalPredecessor = ensureDynamicReadVersion(txn)
      val res =
        if !selfOrFinalPredecessor.isStable then {
          sleepForVersionProperty(
            NonblockingSkipListVersionHistory.stableSleeperUpdate,
            _.isStable,
            selfOrFinalPredecessor,
            "stable"
          )
          selfOrFinalPredecessor.previousWriteIfStable.readForFuture
        } else if selfOrFinalPredecessor.txn == txn then {
          selfOrFinalPredecessor.previousWriteIfStable.readForFuture
        } else {
          selfOrFinalPredecessor.readForFuture
        }
      if NonblockingSkipListVersionHistory.TRACE_VALUES then
        println(
          s"[${Thread.currentThread().getName}] dynamicBefore for $txn ended on $selfOrFinalPredecessor, returning $res"
        )
//    perThreadReadTracker.set(() => s"dynamicBefore for $txn ended on $stableOwnOrPredecessor, returning $res")
      res
    } catch {
      case t: Throwable =>
        val e = new Exception("dynamicBefore failed; printing because reevaluation might catch the result:", t)
        e.printStackTrace()
        throw e
    }

  private def sleepForVersionProperty(
      sleeperUpdater: AtomicReferenceFieldUpdater[LinkWithCounters[Any], List[Thread]],
      check: QueuedVersion => Boolean,
      version: QueuedVersion,
      label: String
  ): Unit = {
    if version.addSleeper(sleeperUpdater) then {
      ForkJoinPool.managedBlock(new ManagedBlocker() {
        override def isReleasable: Boolean = check(version)
        override def block(): Boolean = {
          if NonblockingSkipListVersionHistory.DEBUG || FullMVUtil.DEBUG then
            println(s"[${Thread.currentThread().getName}] parking for $label $version.")
          val timeBefore = System.nanoTime()
          LockSupport.parkNanos(NonblockingSkipListVersionHistory.this, 3000L * 1000 * 1000)
          val res = check(version)
          if System.nanoTime() - timeBefore > 2000L * 1000 * 1000 then {
            if res then {
              val ff = firstFrame
              if ff != version && (ff == null || ff.txn.isTransitivePredecessor(version.txn)) then {
                throw new Exception(
                  s"${Thread.currentThread().getName} got stuck on $version -> dropped without $label $version with state $this"
                )
              } else {
//                System.err.println(s"[WARNING] ${Thread.currentThread().getName} stalled waiting for transition to stable of $version\r\n[WARNING] with state $this\r\n[WARNING]\tat ${Thread.currentThread().getStackTrace.mkString("\r\n[WARNING]\tat ")}")
              }
            } else {
              new Exception(
                s"${Thread.currentThread().getName} did not receive wake-up call after transition to $label of $version with state $this"
              ).printStackTrace()
            }
          }
          res
        }
      })
      if NonblockingSkipListVersionHistory.DEBUG || FullMVUtil.DEBUG then
        println(s"[${Thread.currentThread().getName}] unparked on $label $version")
    } else {
      assert(check(version), s"$label sleeper addition failed, but not $label: $version")
    }
  }

  def staticBefore(txn: T): V =
    try {
      // start at latest stable
      var predecessor = laggingLatestStable.get
      if NonblockingSkipListVersionHistory.DEBUG && NonblockingSkipListVersionHistory.TRACE_VALUES then
        println(s"[${Thread.currentThread().getName}] staticBefore for $txn starting from $predecessor")
      // hop back in time, ignoring all writes that are considered in the future of txn or of txn itself
      while predecessor.txn == txn || lessThanAssumingNoRaceConditions(txn, predecessor.txn) do
        predecessor = predecessor.previousWriteIfStable
      val res = predecessor.readForFuture
      if NonblockingSkipListVersionHistory.TRACE_VALUES then
        println(s"[${Thread.currentThread().getName}] staticBefore for $txn ended at $predecessor, returning $res")
//    perThreadReadTracker.set(() => s"staticBefore for $txn ended on $predecessor, returning $res")
      res
    } catch {
      case t: Throwable =>
        val e = new Exception("staticBefore failed; printing because reevaluation will catch the result:", t)
        e.printStackTrace()
        throw e
    }

  /** entry point for after(this); may suspend.
    *
    * @param txn the executing transaction
    * @return the corresponding value from after this transaction, i.e., awaiting and returning the
    *         transaction's own write if one has occurred or will occur.
    */
  def dynamicAfter(txn: T): V =
    try {
      val selfOrFinalPredecessor = ensureDynamicReadVersion(txn)
      val res =
        if selfOrFinalPredecessor.txn == txn then {
          if !selfOrFinalPredecessor.isFinal then {
            sleepForVersionProperty(
              NonblockingSkipListVersionHistory.finalSleeperUpdate,
              _.isFinal,
              selfOrFinalPredecessor,
              "final"
            )
          }
          if selfOrFinalPredecessor.isWritten then {
            selfOrFinalPredecessor.readForSelf
          } else {
            selfOrFinalPredecessor.previousWriteIfStable.readForFuture
          }
        } else {
          selfOrFinalPredecessor.readForFuture
        }
      if NonblockingSkipListVersionHistory.TRACE_VALUES then
        println(
          s"[${Thread.currentThread().getName}] dynamicAfter for $txn ended on $selfOrFinalPredecessor, returning $res"
        )
//    perThreadReadTracker.set(() => s"dynamicAfter for $txn ended on $stableOwnOrPredecessor, returning $res")
      res
    } catch {
      case t: Throwable =>
        val e = new Exception("dynamicAfter failed; printing because reevaluation might catch the result:", t)
        e.printStackTrace()
        throw e
    }

  def staticAfter(txn: T): V =
    try {
      var ownOrPredecessor = laggingLatestStable.get
      if NonblockingSkipListVersionHistory.DEBUG && NonblockingSkipListVersionHistory.TRACE_VALUES then
        println(s"[${Thread.currentThread().getName}] staticAfter for $txn starting from $ownOrPredecessor")
      // start at latest write
      if !ownOrPredecessor.isWritten then ownOrPredecessor = ownOrPredecessor.previousWriteIfStable
      // hop back in time over all writes that are considered in the future of txn
      while ownOrPredecessor.txn != txn && lessThanAssumingNoRaceConditions(txn, ownOrPredecessor.txn) do
        ownOrPredecessor = ownOrPredecessor.previousWriteIfStable
      // dispatch read
      val res =
        if ownOrPredecessor.txn == txn then {
          ownOrPredecessor.readForSelf
        } else {
          ownOrPredecessor.readForFuture
        }
      if NonblockingSkipListVersionHistory.TRACE_VALUES then
        println(
          s"[${Thread.currentThread().getName}] staticAfter for $txn ended at $ownOrPredecessor, returning $res"
        )
//    perThreadReadTracker.set(() => s"staticAfter for $txn ended on $ownOrPredecessor, returning $res")
      res
    } catch {
      case t: Throwable =>
        val e = new Exception("staticAfter failed; printing because reevaluation will likely catch the result:", t)
        e.printStackTrace()
        throw e
    }

  // =================== DYNAMIC OPERATIONS ====================

  @elidable(elidable.ASSERTION)
  private def assertFinalVersionExists(txn: T, current: QueuedVersion): Unit = assertFinalVersionExists0(txn, current)
  @tailrec private def assertFinalVersionExists0(txn: T, current: QueuedVersion): Unit = {
    if current == null then {
      val ls = laggingLatestStable.get()
      if ls.txn == txn || txn.isTransitivePredecessor(ls.txn) || ls.txn.phase == TurnPhase.Completed then {
        assertFinalVersionExists0(txn, ls)
      } else {
//        assert(ls.txn.isTransitivePredecessor(txn), s"trying to look for $txn encountered unordered latestStable $ls; read tracker says ${perThreadReadTracker.get()()}")
        assert(
          ls.txn.isTransitivePredecessor(txn),
          s"trying to look for $txn encountered unordered latestStable $ls; read tracker disabled..."
        )
      }
    } else if current.txn == txn then {
//      assert(current.isFinal, s"found non-final $current; read tracker says ${perThreadReadTracker.get()()}")
      assert(current.isFinal, s"found non-final $current; read tracker disabled...")
    } else {
//      assert(!current.txn.isTransitivePredecessor(txn), s"while looking for $txn, encountered successor $current; read tracker says ${perThreadReadTracker.get()()}")
//      assert(txn.isTransitivePredecessor(current.txn) || current.txn.phase == TurnPhase.Completed, s"while looking for $txn, encountered unordered $current; read tracker says ${perThreadReadTracker.get()()}")
      assert(
        !current.txn.isTransitivePredecessor(txn),
        s"while looking for $txn, encountered successor $current; read tracker disabled..."
      )
      assert(
        txn.isTransitivePredecessor(current.txn) || current.txn.phase == TurnPhase.Completed,
        s"while looking for $txn, encountered unordered $current; read tracker disabled..."
      )
      val next = current.get
      if next == current then {
        // search fell off the list, restart
        assertFinalVersionExists0(txn, null)
      } else {
//        assert(next != null, s"reached end of list without finding version of $txn; read tracker says ${perThreadReadTracker.get()()}")
        assert(next != null, s"reached end of list without finding version of $txn; read tracker disabled...")
        assertFinalVersionExists0(txn, next)
      }
    }
  }

  def discover(txn: T, add: OutDep): (List[T], Option[T]) = {
    // if this is a trailing edge change (i.e., ordered at the end of the history),
    // the required a marker for future writes to order themselves behind it should have been
    // created previously already, by the preceding dynamicAfter of DynamicTicket.depend().
    // should some method be introduced that executes a discover WITHOUT a prior staticAfter,
    // then this method needs the same protection current implemented in drop(txn, x).
    assertFinalVersionExists(txn, null)
    computeRetrofit(
      txn,
      add,
      synchronized {
        outgoings += add
        (laggingLatestStable.get, firstFrame)
      }
    )
  }

  def drop(txn: T, remove: OutDep): (List[T], Option[T]) = {
    val maybeVersion = ensureDynamicReadVersion(txn, assertSleepingAllowed = false)
//    assert(maybeV ersion.isFinal, s"found non-final $txn; read tracker says ${perThreadReadTracker.get()()}")
    assert(maybeVersion.isFinal, s"found non-final $txn; read tracker disabled...")
    computeRetrofit(
      txn,
      remove,
      synchronized {
        outgoings -= remove
        (laggingLatestStable.get, firstFrame)
      }
    )
  }

  private def computeRetrofit(
      txn: T,
      sink: OutDep,
      latestStableAndFirstFrame: (QueuedVersion, QueuedVersion)
  ): (List[T], Option[T]) = {
    val maybeFirstFrame = latestStableAndFirstFrame._2
    val frame =
      if maybeFirstFrame != null then {
        // TODO in distributed, test if we can turn this into a positive lessThanAssumingNoRaceConditions(txn, maybeFirstFrame.txn).
//      assert(txn != maybeFirstFrame.txn && !txn.isTransitivePredecessor(maybeFirstFrame.txn), s"$txn must not retrofit in the future (firstFrame was $maybeFirstFrame).\r\nSource (this) state is: $this\r\nSink $sink state is: ${sink.asInstanceOf[rescala.core.Reactive[FullMVStruct]].state}\r\n, readTracker says ${perThreadReadTracker.get()}")
        assert(
          txn != maybeFirstFrame.txn && !txn.isTransitivePredecessor(maybeFirstFrame.txn),
          s"$txn must not retrofit in the future (firstFrame was $maybeFirstFrame).\r\nSource (this) state is: $this\r\nSink $sink state is: ${sink.asInstanceOf[Derived].state}\r\n, readTracker disabled..."
        )
        Some(maybeFirstFrame.txn)
      } else {
        None
      }

    var logLine = latestStableAndFirstFrame._1
    if !logLine.isWritten || logLine == maybeFirstFrame then logLine = logLine.previousWriteIfStable
    var retrofits: List[T] = Nil
    while logLine.txn == txn || lessThanAssumingNoRaceConditions(txn, logLine.txn) do {
      retrofits ::= logLine.txn
      logLine = logLine.previousWriteIfStable
    }
    (retrofits, frame)
  }

  /** performs the reframings on the sink of a discover(n, this) with arity +1, or drop(n, this) with arity -1
    *
    * @param successorWrittenVersions the reframings to perform for successor written versions
    * @param maybeSuccessorFrame      maybe a reframing to perform for the first successor frame
    * @param arity                    +1 for discover adding frames, -1 for drop removing frames.
    */
  def retrofitSinkFrames(successorWrittenVersions: Seq[T], maybeSuccessorFrame: Option[T], arity: Int): Seq[T] = {
    require(math.abs(arity) == 1)
//    @tailrec def checkFrames(current: List[T]): Unit = {
//      current match {
//        case Nil =>
//        case first :: Nil => if(maybeSuccessorFrame.isDefined) {
//          assert(maybeSuccessorFrame.get.isTransitivePredecessor(first), s"not $first < $maybeSuccessorFrame")
//        }
//        case first :: rest =>
//          val second :: _ = rest
//          assert(second.isTransitivePredecessor(current.head), s"not $first < $second")
//          checkFrames(rest)
//      }
//    }
//    checkFrames(successorWrittenVersions)

    var current = firstFrame
    val res = successorWrittenVersions.filter { txn =>
      val version =
        if current.txn == txn then {
          // can only occur for successorWrittenVersions.head
          current
        } else {
          assert(
            !current.txn.isTransitivePredecessor(txn),
            s"somehow, current $current > next retrofit $txn (write retrofits are $successorWrittenVersions)"
          )
          var version: QueuedVersion = null
          while version == null do version = enqueueNotifying(txn, current)
          current = version
          version
        }
      // note: if drop retrofitting overtook a change notification, changed may update from 0 to -1 here!
      if arity == +1 then {
        version.incrementChanged() == 1
      } else {
        version.decrementChanged() == 0
      }
    }

    if maybeSuccessorFrame.isDefined then {
      val txn = maybeSuccessorFrame.get
      val version =
        if current.txn == txn then {
          // can only occur if successorWrittenVersions.isEmpty
          current
        } else {
          assert(
            !current.txn.isTransitivePredecessor(txn),
            s"somehow, current $current > frame retrofit $txn (write retrofits are $successorWrittenVersions)"
          )
          var version: QueuedVersion = null
          while version == null do version = enqueueFollowFraming(txn, current)
          version
        }
      // note: conversely, if a (no)change notification overtook discovery retrofitting, pending may change
      // from -1 to 0 here. No handling is required for this case, because firstFrame < position is an active
      // reevaluation (the one that's executing the discovery) and will afterwards progressToNextWrite, thereby
      // executing this then-ready reevaluation, but for now the version is guaranteed not stable yet.
      if arity == +1 then {
        version.incrementPending()
        ()
      } else {
        version.decrementPending()
        ()
      }
    }

    res
  }

  private def tryInsertVersion(
      txn: T,
      current: QueuedVersion,
      next: QueuedVersion,
      previousWriteIfStable: QueuedVersion,
      value: MaybeWritten[V]
  ): QueuedVersion = {
    assert(current.txn != txn, s"trying to insert duplicate after $current!")
    assert(next == null || next.txn != txn, s"trying to insert duplicate before $current!")
    assert(
      previousWriteIfStable == null || previousWriteIfStable.isWritten,
      s"cannot set unwritten stable $previousWriteIfStable"
    )
    assert(
      txn.isTransitivePredecessor(current.txn) || current.txn.phase == TurnPhase.Completed,
      s"inserting version for $txn after unordered $current"
    )
    assert(
      next == null || next.txn.isTransitivePredecessor(txn),
      s"inserting version for $txn before unordered $next"
    )
    val waiting = new QueuedVersion(txn, previousWriteIfStable, value, next)
    if current.compareAndSet(next, waiting) then {
      waiting
    } else {
      null
    }
  }

  override def toString: String = toString(null, null)

  @tailrec private def toString(builder: StringBuilder, current: QueuedVersion): String = {
    if builder == null then {
      val builder =
        new StringBuilder(s"[Before] firstFrame = ").append(firstFrame).append(", outgoings = ").append(outgoings)
      toString(builder, laggingLatestStable.get)
    } else if current == null then {
      builder.append("\r\n[After] firstFrame = ").append(firstFrame).append(", outgoings = ").append(
        outgoings
      ).toString()
    } else {
      builder.append("\r\n\t")
      builder.append(current)
      val next = current.get()
      if next == current then {
        // fell of the list
        toString(null, null)
      } else {
        toString(builder, next)
      }
    }
  }
}

object NonblockingSkipListVersionHistory {
  val DEBUG        = false
  val TRACE_VALUES = false

  val pendingUpdate = AtomicIntegerFieldUpdater.newUpdater(classOf[LinkWithCounters[Any]], "pending")
  val changedUpdate = AtomicIntegerFieldUpdater.newUpdater(classOf[LinkWithCounters[Any]], "changed")
  val stableSleeperUpdate =
    AtomicReferenceFieldUpdater.newUpdater(classOf[LinkWithCounters[Any]], classOf[List[Thread]], "stableSleepers")
  val finalSleeperUpdate =
    AtomicReferenceFieldUpdater.newUpdater(classOf[LinkWithCounters[Any]], classOf[List[Thread]], "finalSleepers")

  /** @param attemptPredecessor intended predecessor transaction
    * @param succToRecord        intended successor transactoin
    * @param defender            encountered transaction
    * @param contender           processing (and thus phase-locked) transaction
    * @return success if relation was established, false if reverse relation was established concurrently
    */
  def tryRecordRelationship(
      attemptPredecessor: FullMVTurn,
      succToRecord: FullMVTurn,
      defender: FullMVTurn,
      contender: FullMVTurn
  ): Boolean = {
    SerializationGraphTracking.tryLock(defender, contender, UnlockedUnknown) match {
      case x: LockedSameSCC =>
        try {
          if succToRecord.isTransitivePredecessor(attemptPredecessor) then {
            // relation already recorded
            true
          } else if attemptPredecessor.isTransitivePredecessor(succToRecord) then {
            // reverse relation already recorded
            false
          } else {
            val tree = attemptPredecessor.selfNode
            if tree == null then {
              assert(
                attemptPredecessor.phase == TurnPhase.Completed,
                s"$attemptPredecessor selfNode was null but isn't completed?"
              )
              // relation no longer needs recording because predecessor completed concurrently
              true
            } else {
              FullMVUtil.myAwait(succToRecord.addPredecessor(tree), contender.host.timeout)
              // relation newly recorded
              true
            }
          }
        } finally {
          x.unlock()
          ()
        }
      case otherwise =>
        Thread.`yield`()
        if attemptPredecessor.phase == TurnPhase.Completed then {
          // relation no longer needs recording because predecessor completed concurrently
          true
        } else if succToRecord.isTransitivePredecessor(attemptPredecessor) then {
          // relation already recorded
          true
        } else if attemptPredecessor.isTransitivePredecessor(succToRecord) then {
          // reverse relation already recorded
          false
        } else {
          // retry
          tryRecordRelationship(attemptPredecessor, succToRecord, defender, contender)
        }
    }
  }

  /** @param txn must be executing
    * @param succ is attempted to be phase-locked in framing, if not already ordered behind txn
    * @return success if order was or became established, failure if it wasn't and succ started executing
    */
  def tryFixSuccessorOrderIfNotFixedYet(txn: FullMVTurn, succ: FullMVTurn): Boolean = {
    succ.isTransitivePredecessor(txn) || {
      val res = FullMVUtil.myAwait(
        succ.acquireRemoteBranchIfPhaseAtMost(TurnPhase.Framing),
        txn.host.timeout
      ) < TurnPhase.Executing
      if res then
        try {
          val established = tryRecordRelationship(txn, succ, succ, txn)
          assert(established, s"failed to order executing $txn before locked-framing $succ?")
        } finally {
          succ.asyncRemoteBranchComplete(TurnPhase.Framing)
        }
      res
    }
  }

  /** @param pred may be framing or executing
    * @param txn   must be phase-locked to framing if pred can be framing
    * @return success if order was or became established, failure if reverse order was or became established concurrently
    */
  def tryFixPredecessorOrderIfNotFixedYet(pred: FullMVTurn, txn: FullMVTurn): Boolean = {
    txn.isTransitivePredecessor(pred) || pred.phase == TurnPhase.Completed || tryRecordRelationship(
      pred,
      txn,
      pred,
      txn
    )
  }
}
