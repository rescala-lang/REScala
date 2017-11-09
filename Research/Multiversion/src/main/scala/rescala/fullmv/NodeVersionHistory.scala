package rescala.fullmv

import java.util.concurrent.ForkJoinPool
import java.util.concurrent.ForkJoinPool.ManagedBlocker

import rescala.core.ValuePersistency
import rescala.fullmv.sgt.synchronization.SubsumableLock

import scala.annotation.elidable.ASSERTION
import scala.annotation.{elidable, tailrec}
import scala.collection.mutable.ArrayBuffer

sealed trait FramingBranchResult[+T, +R]
object FramingBranchResult {
  case object FramingBranchEnd extends FramingBranchResult[Nothing, Nothing]
  case class Frame[T, R](out: Set[R], frame: T) extends FramingBranchResult[T, R]
  case class FrameSupersede[T, R](out: Set[R], frame: T, supersede: T) extends FramingBranchResult[T, R]
  case class Deframe[T, R](out: Set[R], deframe: T) extends FramingBranchResult[T, R]
  case class DeframeReframe[T, R](out: Set[R], deframe: T, reframe: T) extends FramingBranchResult[T, R]
}

sealed trait NotificationResultAction[+T, +R]
object NotificationResultAction {
  // upon notify:
  //    branch merge: T/F
  //    reev: wait/ready/unchanged/unchanged+FF/unchanged+next
  // upon reevOut:
  //    done/FF/next
  case object NotGlitchFreeReady extends NotificationResultAction[Nothing, Nothing]
  case object ResolvedNonFirstFrameToUnchanged extends NotificationResultAction[Nothing, Nothing]
  case object GlitchFreeReadyButQueued extends NotificationResultAction[Nothing, Nothing]
  case object GlitchFreeReady extends NotificationResultAction[Nothing, Nothing]
  sealed trait NotificationOutAndSuccessorOperation[+T, R] extends NotificationResultAction[T, R] {
    val out: Set[R]
  }
  object NotificationOutAndSuccessorOperation {
    case class NoSuccessor[R](out: Set[R]) extends NotificationOutAndSuccessorOperation[Nothing, R]
    case class FollowFraming[T, R](out: Set[R], succTxn: T) extends NotificationOutAndSuccessorOperation[T, R]
    case class NextReevaluation[T, R](out: Set[R], succTxn: T) extends NotificationOutAndSuccessorOperation[T, R]
  }
}

/**
  * A node version history datastructure
  * @param init the initial creating transaction
  * @param valuePersistency the value persistency descriptor
  * @tparam V the type of stored values
  * @tparam T the type of transactions
  * @tparam InDep the type of incoming dependency nodes
  * @tparam OutDep the type of outgoing dependency nodes
  */
class NodeVersionHistory[V, T <: FullMVTurn, InDep, OutDep](init: T, val valuePersistency: ValuePersistency[V]) extends FullMVState[V, T, InDep, OutDep] {
  override val host: FullMVEngine = init.host

  class Version(val txn: T, var stable: Boolean, var out: Set[OutDep], var pending: Int, var changed: Int, var value: Option[V]) extends ManagedBlocker {
    // txn >= Executing, stable == true, node reevaluation completed changed
    def isWritten: Boolean = changed == 0 && value.isDefined
    // txn <= WrapUp, any following versions are stable == false
    def isFrame: Boolean = pending > 0 || changed > 0
    // isReadOrDynamic: has no implications really..
    def isReadOrDynamic: Boolean = pending == 0 && changed == 0 && value.isEmpty
    // isOvertakeCompensation: Will become isReadOrDynamic or isFrame once overtaken (no)change notifications have arrived.
    def isOvertakeCompensation: Boolean = pending < 0 || changed < 0

    // should only be used if isFrame == true is known (although it implies that)
    def isReadyForReevaluation: Boolean = pending == 0 && changed > 0
    // should only be used if txn >= Executing, as it may falsely return true in the case that a txn == Framing
    // had a frame converted into a marker due to frame superseding (or drop retrofitting?) and that frame was
    // marked stable after all preceding placeholders were removed but anoter txn2 == Framing inserts another
    // preceding frame which destabilizes this version again.
    def isFinal: Boolean = isWritten || (isReadOrDynamic && stable)

    def read(): V = {
      assert(isWritten, "reading un-written "+this)
      value.get
    }

    var finalWaiters: Int = 0
    var stableWaiters: Int = 0
    override def block(): Boolean = NodeVersionHistory.this.synchronized {
      isReleasable || {
        finalWaiters += 1
        NodeVersionHistory.this.wait()
        finalWaiters -= 1
        isReleasable }
    }

    override def isReleasable: Boolean = NodeVersionHistory.this.synchronized {
      isFinal
    }

    // common blocking case (now, dynamicDepend): Use self as blocker instance to reduce garbage
    def blockForFinal: ManagedBlocker = this

    // less common blocking case
    // fake lazy val without synchronization, because it is accessed only while the node's monitor is being held.
    private var _blockForStable: ManagedBlocker = null
    def blockForStable: ManagedBlocker = {
      if(_blockForStable == null) {
        _blockForStable = new ManagedBlocker {
          override def block(): Boolean = NodeVersionHistory.this.synchronized {
            isReleasable || {
              stableWaiters += 1
              NodeVersionHistory.this.wait()
              stableWaiters -= 1
              isReleasable }
          }
          override def isReleasable: Boolean = NodeVersionHistory.this.synchronized {
            stable
          }
        }
      }
      _blockForStable
    }


    override def toString: String = {
      if(isWritten){
        s"Written($txn, out=$out, v=${value.get})"
      } else if (isReadOrDynamic) {
        (if(stable) "Stable" else "Unstable") + s"Marker($txn, out=$out)"
      } else if (isOvertakeCompensation) {
        s"OvertakeCompensation($txn, ${if (stable) "stable" else "unstable"}, out=$out, pending=$pending, changed=$changed)"
      } else if(isFrame) {
        if(stable) {
          if(isReadyForReevaluation) {
            s"Active($txn, out=$out)"
          } else {
            s"FirstFrame($txn, out=$out, pending=$pending, changed=$changed)"
          }
        } else {
          if(isReadyForReevaluation) {
            s"Queued($txn, out=$out)"
          } else {
            s"Frame($txn, out=$out, pending=$pending, changed=$changed)"
          }
        }
      } else {
        "UnknownVersionCase!(" + txn + ", " + (if(stable) "stable" else "unstable") + ", out=" + out + ", pending=" + pending + ", changed=" + changed + ", value = " + value + ")"
      }
    }
  }

  @elidable(ASSERTION) @inline
  def assertOptimizationsIntegrity(debugOutputDescription: => String): Unit = {
    def debugStatement(whatsWrong: String): String = s"$debugOutputDescription left $whatsWrong in $this"

    assert(size <= _versions.length, debugStatement("size out of bounds"))
    assert(size > 0, debugStatement("version list empty"))
    assert(!_versions.take(size).contains(null), debugStatement("null version in bounds"))
    assert(!_versions.drop(size).exists(_ != null), debugStatement("non-null version outside bounds"))
    assert(_versions(0).isWritten, debugStatement("first version not written"))

    assert(firstFrame > 0, debugStatement("firstFrame out of bounds negative"))
    assert(firstFrame <= size, debugStatement("firstFrame out of bounds positive"))
    assert(firstFrame == size || _versions(firstFrame).isFrame, debugStatement("firstFrame not frame"))
    assert(!_versions.take(firstFrame).exists(_.isFrame), debugStatement("firstFrame not first"))

    assert(latestWritten >= 0, debugStatement("latestWritten out of bounds negative"))
    assert(latestWritten < size, debugStatement("latestWritten out of bounds positive"))
    assert(_versions(latestWritten).isWritten, debugStatement("latestWritten not written"))
    assert(!_versions.slice(latestWritten + 1, size).exists(_.isWritten), debugStatement("latestWritten not latest"))

    assert(!_versions.take(size).zipWithIndex.exists{case (version, index) => version.stable != (index <= firstFrame)}, debugStatement("broken version stability"))
  }

  override def toString: String = super.toString + s" -> size $size, latestWritten $latestWritten, ${if(firstFrame > size) "no frames" else "firstFrame "+firstFrame}): \n  " + _versions.zipWithIndex.map{case (version, index) => s"$index: $version"}.mkString("\n  ")

  // =================== STORAGE ====================

  var _versions = new Array[Version](11)
  _versions(0) = new Version(init, stable = true, out = Set(), pending = 0, changed = 0, Some(valuePersistency.initialValue))
  var size = 1
  var latestValue: V = valuePersistency.initialValue

  private def createHole(position: Int): Unit = {
    if (_versions.length == size) {
      val newVersions = new Array[Version](size + size / 2)
      System.arraycopy(_versions, 0, newVersions, 0, position)
      System.arraycopy(_versions, position, newVersions, position + 1, size - position)
      _versions = newVersions
    } else {
      System.arraycopy(_versions, position, _versions, position + 1, size - position)
    }
  }

  private def createVersionInHole(position: Int, txn: T) = {
    val version = new Version(txn, stable = position <= firstFrame, _versions(position - 1).out, pending = 0, changed = 0, None)
    size += 1
    _versions(position) = version
    if (position <= firstFrame) firstFrame += 1
    if (position <= latestWritten) latestWritten += 1
    version
  }

  // =================== NAVIGATION ====================
  var firstFrame: Int = size
  var latestWritten: Int = 0

  /**
    * performs binary search for the given transaction in _versions. This establishes an order in sgt against all other
    * versions' transactions, placing the given transaction as late as possible with respect to transaction's current
    * phases. If asked, a new version is created at the found position. May expunge any number of obsolete versions.
    * @param lookFor the transaction to look for
    * @param from current from index of the search range
    * @param to current to index (exclusive) of the search range
    * @param versionRequired whether or not a version should be created, in case none exists for the given transaction
    * @return a tuple consisting of: (a) the index of the version associated with the given transaction, or if no such
    *         version exists and was not requested to be created, the negative index at which it would have to be
    *         inserted (b) how many versions were expunged for garbage collection. Note that we assume no insertion to
    *         ever occur at index 0 -- the the first version of any node is either from the transaction that created it
    *         or from some completed transaction. In the former case, no preceding transaction should be aware of the
    *         node's existence. In the latter case, there are no preceding transactions that still execute operations.
    *         Thus insertion at index 0 should be impossible. A return value of 0 thus means "found at 0", rather than
    *         "should insert at 0"
    */
  private def findOrPigeonHole(lookFor: T, from: Int, to: Int, versionRequired: Boolean): (Int, Int) = {
    assert(from <= to, s"binary search started with backwards indices from $from to $to")
    assert(to <= size, s"binary search upper bound $to beyond history size $size")
    assert(to == size || Set[PartialOrderResult](SecondFirstSameSCC, UnorderedSCCUnknown).contains(DecentralizedSGT.getOrder(_versions(to).txn, lookFor)), s"to = $to successor for non-blocking search of known static $lookFor pointed to version of ${_versions(to).txn} which is ordered earlier.")
    assert(DecentralizedSGT.getOrder(_versions(from - 1).txn, lookFor) != SecondFirstSameSCC, s"from - 1 = ${from - 1} predecessor for non-blocking search of $lookFor pointed to version of ${_versions(from - 1).txn} which is already ordered later.")
    assert(from > 0, s"binary search started with non-positive lower bound $from")

    val (posOrInsert, canGCupto) = findOrPigeonHoleNonblocking(lookFor, from, fromIsKnownPredecessor = false, to, 0, knownSameSCC = false)

    assert(_versions(canGCupto).txn.phase == TurnPhase.Completed, s"binary search returned $posOrInsert for $lookFor with GC hint $canGCupto pointing to a non-completed transaction in $this")
    assert(canGCupto < math.abs(posOrInsert), s"binary search returned $posOrInsert for $lookFor inside garbage collected section (< $canGCupto) in $this")

    assert(posOrInsert < size, s"binary search returned found at $posOrInsert for $lookFor, which is out of bounds in $this")
    assert(posOrInsert < 0 || _versions(posOrInsert).txn == lookFor, s"binary search returned found at $posOrInsert for $lookFor, which is wrong in $this")
    assert(posOrInsert >= 0 || -posOrInsert <= size, s"binary search returned insert at ${-posOrInsert}, which is out of bounds in $this")
    assert(posOrInsert >= 0 || Set[PartialOrderResult](FirstFirstSameSCC, FirstFirstSCCUnkown).contains(DecentralizedSGT.getOrder(_versions(-posOrInsert - 1).txn, lookFor)), s"binary search returned insert at ${-posOrInsert} for $lookFor, but predecessor isn't ordered first in $this")
    assert(posOrInsert >= 0 || -posOrInsert == to || DecentralizedSGT.getOrder(_versions(-posOrInsert).txn, lookFor) == SecondFirstSameSCC, s"binary search returned insert at ${-posOrInsert} for $lookFor, but it isn't ordered before successor in $this")

    // compute a possible location for gc to leave a hole
    val mustCreateVersion = versionRequired && posOrInsert < 0
    val holeLocation = if (mustCreateVersion) -posOrInsert else size

    // maybe perform gc and leave said hole
    val gcdFromHint = if(canGCupto > 0) {
      val gcd = gcBeforeLeaveHole(canGCupto, holeLocation)
//      if(gcd == 0) assertOptimizationsIntegrity(s"hinted GC after search($lookFor, ${from - gcd}, ${to - gcd})->($canGCupto, $posOrInsert)")
      gcd
    } else {
      0
    }

    val gcd = if (versionRequired && size == _versions.length) {
      assert(gcdFromHint == 0, s"gc based on search gc hint $canGCupto removed $gcdFromHint versions, but the array supposedly is still at max capacity in $this")
      val gcd = fullGCLeaveHole(holeLocation)
//      if(gcd == 0) assertOptimizationsIntegrity(s"full GC after search($lookFor, ${from - gcd}, ${to - gcd})->($canGCupto, $posOrInsert)")
      gcd
    } else {
      gcdFromHint
    }

    // maybe create required version in the hole, create the hole beforehand if GC didn't
    if(mustCreateVersion) {
      val correctedHoleLocation = holeLocation - gcd
      if(gcd == 0) createHole(correctedHoleLocation)
      createVersionInHole(correctedHoleLocation, lookFor)
//      if(gcd != 0) assertOptimizationsIntegrity(s"search($lookFor, ${from - gcd}, ${to - gcd})->($canGCupto, $posOrInsert)+gcd($gcd)+insertion($correctedHoleLocation)")
      (correctedHoleLocation, gcd)
    } else if (posOrInsert < 0) {
      (posOrInsert + gcd, gcd)
    } else {
      (posOrInsert - gcd, gcd)
    }
  }

  @tailrec
  private def findOrPigeonHoleNonblocking(lookFor: T, from: Int, fromIsKnownPredecessor: Boolean, to: Int, canGCupto: Int, knownSameSCC: Boolean): (Int, Int) = {
    if (to == from) {
      if(!fromIsKnownPredecessor) {
        val pred = _versions(from - 1).txn
        val unblockedOrder = DecentralizedSGT.getOrder(pred, lookFor)
        assert(unblockedOrder != SecondFirstSameSCC)
        if(unblockedOrder == UnorderedSCCUnknown) {
          val maybeLockedRoot = if (knownSameSCC) {
            Some(SubsumableLock.acquireLock(lookFor, host.timeout))
          } else {
            SubsumableLock.acquireLock(pred, lookFor, host.timeout)
          }
          maybeLockedRoot match {
            case Some(lockedRoot) =>
              try {
                val establishedOrder = DecentralizedSGT.ensureOrder(pred, lookFor, host.timeout)
                assert(establishedOrder == FirstFirst)
              } finally { lockedRoot.asyncUnlock() }
            case None =>
              assert(DecentralizedSGT.getOrder(pred, lookFor) == FirstFirstSCCUnkown, s"establishing order under lock failed due to opponent completing, but completion did not induce implicit ordering")
          }
        }
      }
      (-from, canGCupto)
    } else {
      val idx = from+(to-from-1)/2
      val candidate = _versions(idx).txn
      val nextGcUpto = if(candidate.phase == TurnPhase.Completed) idx else canGCupto
      if(candidate == lookFor) {
        (idx, canGCupto)
      } else DecentralizedSGT.getOrder(candidate, lookFor) match {
        case FirstFirstSCCUnkown =>
          findOrPigeonHoleNonblocking(lookFor, idx + 1, fromIsKnownPredecessor = true, to, nextGcUpto, knownSameSCC)
        case FirstFirstSameSCC =>
          findOrPigeonHoleNonblocking(lookFor, idx + 1, fromIsKnownPredecessor = true, to, nextGcUpto, knownSameSCC = true)
        case SecondFirstSameSCC =>
          findOrPigeonHoleNonblocking(lookFor, from, fromIsKnownPredecessor, idx, nextGcUpto, knownSameSCC = true)
        case UnorderedSCCUnknown =>
          val lockedRoot = SubsumableLock.acquireLock(lookFor, host.timeout)
          if(knownSameSCC) {
            try {
              findOrPigeonHoleLocked(lookFor, from, fromIsKnownPredecessor, to, nextGcUpto)
            } finally { lockedRoot.asyncUnlock() }
          } else {
            SubsumableLock.trySubsumeDefender(lockedRoot, candidate, host.timeout) match {
              case Some(newLockedRoot) =>
                try {
                  findOrPigeonHoleLocked(lookFor, from, fromIsKnownPredecessor, to, nextGcUpto)
                } finally { newLockedRoot.asyncUnlock() }
              case None =>
                assert(DecentralizedSGT.getOrder(candidate, lookFor) == FirstFirstSCCUnkown, s"continuing search under lock failed due to entry opponent completing, but completion did not induce implicit ordering")
                findOrPigeonHoleNonblocking(lookFor, idx + 1, fromIsKnownPredecessor = true, to, nextGcUpto, knownSameSCC)
            }
          }
      }
    }
  }

  @tailrec
  private def findOrPigeonHoleLocked(lookFor: T, from: Int, fromKnownOrdered: Boolean, to: Int, canGCupto: Int): (Int, Int) = {
    if (to == from) {
      if(!fromKnownOrdered) {
        val pred = _versions(from - 1).txn
        val establishedOrder = DecentralizedSGT.ensureOrder(pred, lookFor, host.timeout)
        assert(establishedOrder == FirstFirst)
      }
      (-from, canGCupto)
    } else {
      val idx = from+(to-from-1)/2
      val candidate = _versions(idx).txn
      val nextGcUpto = if(candidate.phase == TurnPhase.Completed) idx else canGCupto
      if(candidate == lookFor) {
        (idx, canGCupto)
      } else DecentralizedSGT.ensureOrder(candidate, lookFor, host.timeout) match {
        case FirstFirst =>
          findOrPigeonHoleLocked(lookFor, idx + 1, fromKnownOrdered = true, to, nextGcUpto)
        case SecondFirst =>
          findOrPigeonHoleLocked(lookFor, from, fromKnownOrdered, idx, nextGcUpto)
      }
    }
  }

  /**
    * determine the position or insertion point for a framing transaction
    *
    * @param txn the transaction
    * @return the position (positive values) or insertion point (negative values)
    */
  private def getFramePositionFraming(txn: T, minPos: Int): (Int, Int) = {
    findOrPigeonHole(txn, minPos, size, versionRequired = true)
  }

  private def getFramePositionPropagating(txn: T): (Int, Int) = {
    if(firstFrame < size && _versions(firstFrame).txn == txn)
      // common-case shortcut attempt: receive notification for firstFrame
      (firstFrame, 0)
    else
      getFramePositionFraming(txn, latestWritten + 1)
  }

  /**
    * determine the position or insertion point for a transaction for which
    * this node is known to have become final
    *
    * @param txn the transaction
    * @return the position (positive values) or insertion point (negative values)
    */
  private def findFinalPosition/*Propagating*/(txn: T, versionRequired: Boolean): (Int, Int) = {
    if(_versions(latestWritten).txn == txn)
      // common-case shortcut attempt: read latest written value
      (latestWritten, 0)
    else
      findOrPigeonHole(txn, 1, firstFrame, versionRequired)
  }


  // =================== FRAMING ====================

  /**
    * entry point for regular framing
    *
    * @param txn the transaction visiting the node for framing
    */
  override def incrementFrame(txn: T): FramingBranchResult[T, OutDep] = synchronized {
    val result = incrementFrame0(txn)
    assertOptimizationsIntegrity(s"incrementFrame($txn) -> $result")
    result
  }

  /**
    * entry point for superseding framing
    * @param txn the transaction visiting the node for framing
    * @param supersede the transaction whose frame was superseded by the visiting transaction at the previous node
    */
  override def incrementSupersedeFrame(txn: T, supersede: T): FramingBranchResult[T, OutDep] = synchronized {
    val position = getFramePositionFraming(txn, latestWritten + 1)._1
    val version = _versions(position)
    version.pending += 1
    val result = if(position < firstFrame && _versions(position).pending == 1) {
      val(supersedePos, gcd) = getFramePositionFraming(supersede, position)
      _versions(supersedePos).pending -= 1
      incrementFrameResultAfterNewFirstFrameWasCreated(txn, position - gcd)
    } else {
      decrementFrame0(supersede, position)
    }
    assertOptimizationsIntegrity(s"incrementSupersedeFrame($txn, $supersede) -> $result")
    result
  }

  override def decrementFrame(txn: T): FramingBranchResult[T, OutDep] = synchronized {
    val result = decrementFrame0(txn)
    assertOptimizationsIntegrity(s"decrementFrame($txn) -> $result")
    result
  }

  override def decrementReframe(txn: T, reframe: T): FramingBranchResult[T, OutDep] = synchronized {
    val position = getFramePositionFraming(txn, latestWritten + 1)._1
    val version = _versions(position)
    version.pending += -1
    val result = if(position == firstFrame && version.pending == 0) {
      val(reframePos, gcd) = getFramePositionFraming(reframe, position)
      _versions(reframePos).pending += 1
      deframeResultAfterPreviousFirstFrameWasRemoved(txn, position - gcd)
    } else {
      incrementFrame0(reframe, position)
    }
    assertOptimizationsIntegrity(s"deframeReframe($txn, $reframe) -> $result")
    result
  }

  private def incrementFrame0(txn: T, minPos: Int = latestWritten + 1): FramingBranchResult[T, OutDep] = {
    val position = getFramePositionFraming(txn, minPos)._1
    val version = _versions(position)
    version.pending += 1
    if (position < firstFrame && version.pending == 1) {
      incrementFrameResultAfterNewFirstFrameWasCreated(txn, position)
    } else {
      FramingBranchResult.FramingBranchEnd
    }
  }

  private def decrementFrame0(txn: T, minPos: Int = latestWritten + 1): FramingBranchResult[T, OutDep] = {
    val position = getFramePositionFraming(txn, minPos)._1
    val version = _versions(position)
    version.pending -= 1
    if (position == firstFrame && version.pending == 0) {
      deframeResultAfterPreviousFirstFrameWasRemoved(txn, position)
    } else {
      FramingBranchResult.FramingBranchEnd
    }
  }

  @tailrec private def destabilizeBackwardsUntilFrame(): Unit = {
    if(firstFrame < size) {
      val version = _versions(firstFrame)
      assert(version.stable, s"cannot destabilize $firstFrame: $version")
      version.stable = false
    }
    firstFrame -= 1
    if(!_versions(firstFrame).isFrame) destabilizeBackwardsUntilFrame()
  }

  private def incrementFrameResultAfterNewFirstFrameWasCreated(txn: T, position: Int) = {
    val previousFirstFrame = firstFrame
    destabilizeBackwardsUntilFrame()
    assert(firstFrame == position, s"destablizeBackwards did not reach $position: ${_versions(position)} but stopped at $firstFrame: ${_versions(firstFrame)}")

    if(previousFirstFrame < size) {
      FramingBranchResult.FrameSupersede(_versions(position).out, txn, _versions(previousFirstFrame).txn)
    } else {
      FramingBranchResult.Frame(_versions(position).out, txn)
    }
  }

  private def stabilizeForwardsUntilFrame(): Boolean = {
    @tailrec @inline def stabilizeForwardsUntilFrame0(encounteredWaiter: Boolean): Boolean = {
      firstFrame += 1
      if (firstFrame < size) {
        val version = _versions(firstFrame)
        assert(!version.stable, s"cannot stabilize $firstFrame: $version")
        version.stable = true
        val updatedEncounteredWaiters = encounteredWaiter || version.stableWaiters > 0
        if (!version.isFrame) {
          stabilizeForwardsUntilFrame0(updatedEncounteredWaiters || version.finalWaiters > 0)
        } else {
          updatedEncounteredWaiters
        }
      } else {
        encounteredWaiter
      }
    }
    stabilizeForwardsUntilFrame0(_versions(firstFrame).finalWaiters > 0)
  }

  private def deframeResultAfterPreviousFirstFrameWasRemoved(txn: T, position: Int) = {
    val encounteredWaiters = stabilizeForwardsUntilFrame()
    assert(!encounteredWaiters, "someone was waiting for a version by a framing transaction, but only executing transactions should perform waiting and they should never see framing transactions' versions.")

    if(firstFrame < size) {
      FramingBranchResult.DeframeReframe(_versions(position).out, txn, _versions(firstFrame).txn)
    } else {
      FramingBranchResult.Deframe(_versions(position).out, txn)
    }
  }

  /*
   * =================== NOTIFICATIONS/ / REEVALUATION ====================
   */

  /**
    * entry point for change/nochange notification reception
    * @param txn the transaction sending the notification
    * @param changed whether or not the dependency changed
    */
  override def notify(txn: T, changed: Boolean): NotificationResultAction[T, OutDep] = synchronized {
    val result = notify0(getFramePositionPropagating(txn)._1, txn, changed)
    assertOptimizationsIntegrity(s"notify($txn, $changed) -> $result")
    result
  }

  /**
    * entry point for change/nochange notification reception with follow-up framing
    * @param txn the transaction sending the notification
    * @param changed whether or not the dependency changed
    * @param followFrame a transaction for which to create a subsequent frame, furthering its partial framing.
    */
  override def notifyFollowFrame(txn: T, changed: Boolean, followFrame: T): NotificationResultAction[T, OutDep] = synchronized {
    val position = getFramePositionPropagating(txn)._1

    val (followFramePos, gcd) = findOrPigeonHole(followFrame, position + 1, size, versionRequired = true)
    _versions(followFramePos).pending += 1

    val result = notify0(position - gcd, txn, changed)
    assertOptimizationsIntegrity(s"notifyFollowFrame($txn, $changed, $followFrame) -> $result")
    result
  }

  private def notify0(position: Int, txn: T, changed: Boolean): NotificationResultAction[T, OutDep] = {
    val version = _versions(position)
    // This assertion is probably pointless as it only verifies a subset of assertStabilityIsCorrect, i.e., if this
    // would fail, then assertStabilityIsCorrect will have failed at the end of the previous operation already.
    assert((position == firstFrame) == version.stable, "firstFrame and stable diverted..")

    // note: if the notification overtook a previous turn's notification with followFraming for this transaction,
    // pending may update from 0 to -1 here
    version.pending -= 1
    if (changed) {
      // note: if drop retrofitting overtook the change notification, change may update from -1 to 0 here!
      version.changed += 1
    }

    // check if the notification triggers subsequent actions
    if (version.pending == 0) {
      if (position == firstFrame) {
        if (version.changed > 0) {
          NotificationResultAction.GlitchFreeReady
        } else {
          // ResolvedFirstFrameToUnchanged
          progressToNextWriteForNotification(version)
        }
      } else {
        if (version.changed > 0) {
          NotificationResultAction.GlitchFreeReadyButQueued
        } else {
          NotificationResultAction.ResolvedNonFirstFrameToUnchanged
        }
      }
    } else {
      NotificationResultAction.NotGlitchFreeReady
    }
  }

  override def reevIn(turn: T): V = {
    synchronized {
      val firstFrameTurn = _versions(firstFrame).txn
      assert(firstFrameTurn == turn, s"Turn $turn called reevIn, but Turn $firstFrameTurn is first frame owner")
    }
    latestValue
  }

  /**
    * progress [[firstFrame]] forward until a [[Version.isFrame]] is encountered, and
    * return the resulting notification out (with reframing if subsequent write is found).
    */
  override def reevOut(turn: T, maybeValue: Option[V]): NotificationResultAction.NotificationOutAndSuccessorOperation[T, OutDep] = synchronized {
    val position = firstFrame
    val version = _versions(position)
    assert(version.txn == turn, s"$turn called reevDone, but first frame is $version (different transaction)")
    assert(!version.isWritten, s"$turn cannot write twice: $version")
    assert((version.isFrame && version.isReadyForReevaluation) || (maybeValue.isEmpty && version.isReadOrDynamic), s"$turn cannot write changed=${maybeValue.isDefined} on $version")

    if(maybeValue.isDefined) {
      latestWritten = position
      this.latestValue = maybeValue.get
      version.value = maybeValue
    }
    version.changed = 0

    val result = progressToNextWriteForNotification(version)
    assertOptimizationsIntegrity(s"reevOut($turn, ${maybeValue.isDefined}) -> $result")
    result
  }

  /**
    * progresses [[firstFrame]] forward until a [[Version.isFrame]] is encountered and assemble all necessary
    * information to send out change/nochange notifications for the given transaction. Also capture synchronized,
    * whether or not the possibly encountered write [[Version.isReadyForReevaluation]].
    * @return the notification and next reevaluation descriptor.
    */
  private def progressToNextWriteForNotification(finalizedVersion: Version): NotificationResultAction.NotificationOutAndSuccessorOperation[T, OutDep] = {
    val encounteredWaiters = stabilizeForwardsUntilFrame()
    val res = if(firstFrame < size) {
      val newFirstFrame = _versions(firstFrame)
      if(newFirstFrame.isReadyForReevaluation) {
        NotificationResultAction.NotificationOutAndSuccessorOperation.NextReevaluation(finalizedVersion.out, newFirstFrame.txn)
      } else {
        NotificationResultAction.NotificationOutAndSuccessorOperation.FollowFraming(finalizedVersion.out, newFirstFrame.txn)
      }
    } else {
      NotificationResultAction.NotificationOutAndSuccessorOperation.NoSuccessor(finalizedVersion.out)
    }
    if(encounteredWaiters) notifyAll()
    res
  }

  // =================== READ OPERATIONS ====================

  /**
    * ensures at least a read version is stored to track executed reads or dynamic operations.
    * @param txn the executing transaction
    * @return the version's position.
    */
  private def ensureReadVersion(txn: T, knownMinPos: Int = 1): (Int, Int) = {
    findOrPigeonHole(txn, knownMinPos, size, versionRequired = true)
  }

  /**
    * entry point for before(this); may suspend.
    *
    * @param txn the executing transaction
    * @return the corresponding [[Version.value]] from before this transaction, i.e., ignoring the transaction's
    *         own writes.
    */
  override def dynamicBefore(txn: T): V = synchronized {
    assert(!valuePersistency.isTransient, s"$txn invoked dynamicBefore on transient node")
    synchronized {
      val position = ensureReadVersion(txn)._1
      assertOptimizationsIntegrity(s"ensureReadVersion($txn)")
      val version = _versions(position)
      if(version.stable) {
        Left(before(txn, position))
      } else {
        Right(version)
      }
    } match {
      case Left(value) => value
      case Right(version) =>

        ForkJoinPool.managedBlock(version.blockForStable)

        synchronized {
          val stablePosition = if(version.isFrame) firstFrame else findFinalPosition(txn, versionRequired = true)._1
          assert(stablePosition >= 0, "somehow, the version allocated above disappeared..")
          before(txn, stablePosition)
        }
    }
  }

  override def staticBefore(txn: T): V = synchronized {
    before(txn, math.abs(findFinalPosition(txn, versionRequired = false)._1))
  }

  private def before(txn: T, position: Int): V = synchronized {
    assert(!valuePersistency.isTransient, "before read on transient node")
    assert(position > 0, s"$txn cannot read before first version")
    val lastWrite = if(latestWritten < position) {
      assert(!_versions.slice(latestWritten + 1, position).exists(_.isFrame))
      _versions(latestWritten)
    } else {
      lastWriteUpTo(position - 1)
    }
    lastWrite.value.get
  }

  @tailrec private def lastWriteUpTo(pos: Int): Version = {
    assert(pos >= 0, s"could not find a previous written version, although at least the first version should always be readable")
    val version = _versions(pos)
    assert(!version.isFrame, s"found frame while searching for predecessor version to read -- forgotten dynamic access synchronization?")
    if(version.value.isDefined) {
      version
    } else {
      lastWriteUpTo(pos - 1)
    }
  }

  private def beforeOrInit(txn: T, position: Int): V = {
    if(valuePersistency.isTransient) {
      valuePersistency.initialValue
    } else {
      before(txn, position)
    }
  }

  /**
    * entry point for after(this); may suspend.
    * @param txn the executing transaction
    * @return the corresponding [[Version.value]] from after this transaction, i.e., awaiting and returning the
    *         transaction's own write if one has occurred or will occur.
    */
  override def dynamicAfter(txn: T): V = {
    synchronized {
      val position = ensureReadVersion(txn)._1
      val version = _versions(position)
      if(version.isFinal) {
        Left(if(version.value.isDefined) {
          version.value.get
        } else {
          beforeOrInit(txn, position)
        })
      } else {
        Right(version)
      }
    } match {
      case Left(value) => value
      case Right(version) =>

        ForkJoinPool.managedBlock(version.blockForFinal)

        if(version.value.isDefined) {
          version.value.get
        } else {
          synchronized {
            beforeOrInit(txn, findFinalPosition(txn, versionRequired = true)._1)
          }
        }
    }
  }

  override def staticAfter(txn: T): V = synchronized {
    val position = findFinalPosition(txn, versionRequired = false)._1
    if(position < 0) {
      beforeOrInit(txn, -position)
    } else {
      val version = _versions(position)
      assert(!version.isFrame, s"staticAfter discovered frame $version -- did the caller wrongly assume a statically known dependency?")
      version.value.getOrElse {
        beforeOrInit(txn, position)
      }
    }
  }

  // =================== DYNAMIC OPERATIONS ====================

  /**
    * entry point for discover(this, add). May suspend.
    * @param txn the executing reevaluation's transaction
    * @param add the new edge's sink node
    * @return the appropriate [[Version.value]].
    */
  override def discover(txn: T, add: OutDep): (Seq[T], Option[T]) = synchronized {
    val position = ensureReadVersion(txn)._1
    assertOptimizationsIntegrity(s"ensureReadVersion($txn)")
    assert(!_versions(position).out.contains(add), "must not discover an already existing edge!")
    retrofitSourceOuts(position, add, +1)
  }

  /**
    * entry point for drop(this, ticket.issuer); may suspend temporarily.
    * @param txn the executing reevaluation's transaction
    * @param remove the removed edge's sink node
    */
  override def drop(txn: T, remove: OutDep): (Seq[T], Option[T]) = synchronized {
    val position = ensureReadVersion(txn)._1
    assertOptimizationsIntegrity(s"ensureReadVersion($txn)")
    assert(_versions(position).out.contains(remove), "must not drop a non-existing edge!")
    retrofitSourceOuts(position, remove, -1)
  }

  /**
    * performs the reframings on the sink of a discover(n, this) with arity +1, or drop(n, this) with arity -1
    * @param successorWrittenVersions the reframings to perform for successor written versions
    * @param maybeSuccessorFrame maybe a reframing to perform for the first successor frame
    * @param arity +1 for discover adding frames, -1 for drop removing frames.
    */
  override def retrofitSinkFrames(successorWrittenVersions: Seq[T], maybeSuccessorFrame: Option[T], arity: Int): Unit = synchronized {
    require(math.abs(arity) == 1)
    var minPos = firstFrame
    for(txn <- successorWrittenVersions) {
      val position = ensureReadVersion(txn, minPos)._1
      val version = _versions(position)
      // note: if drop retrofitting overtook a change notification, changed may update from 0 to -1 here!
      version.changed += arity
      minPos = position + 1
    }

    if (maybeSuccessorFrame.isDefined) {
      val txn = maybeSuccessorFrame.get
      val position = ensureReadVersion(txn, minPos)._1
      val version = _versions(position)
      // note: conversely, if a (no)change notification overtook discovery retrofitting, pending may change
      // from -1 to 0 here. No handling is required for this case, because firstFrame < position is an active
      // reevaluation (the one that's executing the discovery) and will afterwards progressToNextWrite, thereby
      // executing this then-ready reevaluation, but for now the version is guaranteed not stable yet.
      version.pending += arity
    }
  }

  /**
    * rewrites all affected [[Version.out]] of the source this during drop(this, delta) with arity -1 or
    * discover(this, delta) with arity +1, and collects transactions for retrofitting frames on the sink node
    * @param position the executing transaction's version's position
    * @param delta the outgoing dependency to add/remove
    * @param arity +1 to add, -1 to remove delta to each [[Version.out]]
    * @return a list of transactions with written successor versions and maybe the transaction of the first successor
    *         frame if it exists, for which reframings have to be performed at the sink.
    */
  private def retrofitSourceOuts(position: Int, delta: OutDep, arity: Int): (Seq[T], Option[T]) = {
    require(math.abs(arity) == 1)
    // allocate array to the maximum number of written versions that might follow
    // (any version at index firstFrame or later can only be a frame, not written)
    val sizePrediction = math.max(firstFrame - position, 0)
    val successorWrittenVersions = new ArrayBuffer[T](sizePrediction)
    for(pos <- position until size) {
      val version = _versions(pos)
      if(arity < 0) version.out -= delta else version.out += delta
      // as per above, this is implied false if pos >= firstFrame:
      if(version.isWritten) successorWrittenVersions += version.txn
    }
    if(successorWrittenVersions.size > sizePrediction) System.err.println(s"FullMV retrofitSourceOuts predicted size max($firstFrame - $position, 0) = $sizePrediction, but size eventually was ${successorWrittenVersions.size}")
    val maybeSuccessorFrame = if (firstFrame < size) Some(_versions(firstFrame).txn) else None
    (successorWrittenVersions, maybeSuccessorFrame)
  }

  def fullGC(): Int = synchronized {
    fullGCLeaveHole(size)
  }

  def fullGCLeaveHole(leaveInsertSlot: Int): Int = {
    if(size > 1) {
      @tailrec @inline def findLastCompleted(from: Int, to: Int): Int = {
        if (to < from) {
          if (_versions(from).txn.phase == TurnPhase.Completed) from else from - 1
        } else {
          val idx = from + (to - from - 1) / 2
          val candidate = _versions(idx).txn
          if (candidate.phase == TurnPhase.Completed) {
            // maybe candidate is last completed
            if (_versions(idx + 1).txn.phase == TurnPhase.Completed) findLastCompleted(idx + 1, to) else idx
          } else {
            // maybe candidate is first uncompleted
            if (_versions(idx - 1).txn.phase == TurnPhase.Completed) idx - 1 else findLastCompleted(from, idx - 1)
          }
        }
      }
      val lastCompleted = if(_versions(firstFrame - 1).txn.phase == TurnPhase.Completed)
        // common case shortcut and corner case: all transactions that can be completed are completed (e.g., graph is in resting state)
        firstFrame - 1
      else
        findLastCompleted(1, firstFrame - 2)

      assert(_versions(lastCompleted).txn.phase == TurnPhase.Completed)
      // cannot make this assertion because the previously last completed turn may already no longer be the last turn at
      // this point, as successive turns might have concurrently completed
      //assert(lastCompleted + 1 == size || _versions(lastCompleted + 1).txn.phase != TurnPhase.Completed)

      if (lastCompleted > 0) gcBeforeLeaveHole(lastCompleted, leaveInsertSlot) else 0
    } else {
      0
    }
  }

  private def gcBeforeLeaveHole(knownCompleted: Int, leaveInsertSlot: Int): Int = {
    assert(leaveInsertSlot <= size)
    assert(knownCompleted > 0)
    val insertSlotIsLast = leaveInsertSlot == size
    if (_versions(knownCompleted).value.isDefined) {
      // if lastCompleted is a written version, then just dump all preceding versions
      firstFrame -= knownCompleted
      latestWritten -= knownCompleted
      val elementsBeforeSlot = leaveInsertSlot - knownCompleted
      System.arraycopy(_versions, knownCompleted, _versions, 0, elementsBeforeSlot)
      if(leaveInsertSlot < size) System.arraycopy(_versions, leaveInsertSlot, _versions, elementsBeforeSlot + 1, size - leaveInsertSlot)
      size -= knownCompleted
      java.util.Arrays.fill(_versions.asInstanceOf[Array[AnyRef]], size + (if (insertSlotIsLast) 0 else 1), _versions.length, null)
      knownCompleted
    } else if (knownCompleted > 1) {
      // if lastCompleted is not a written version, then dump all preceding versions except for the last written one
      val dumpCount = knownCompleted - 1
      firstFrame -= dumpCount
      _versions(0) = if (latestWritten <= knownCompleted && _versions(latestWritten).txn.phase == TurnPhase.Completed) {
        val result = _versions(latestWritten)
        latestWritten = 0
        result
      } else {
        latestWritten -= dumpCount
        lastWriteUpTo(knownCompleted)
      }
      val elementsBeforeSlot = leaveInsertSlot - knownCompleted
      System.arraycopy(_versions, knownCompleted, _versions, 1, elementsBeforeSlot)
      if(leaveInsertSlot < size) System.arraycopy(_versions, leaveInsertSlot, _versions, elementsBeforeSlot + 2, size - leaveInsertSlot)
      size -= dumpCount
      java.util.Arrays.fill(_versions.asInstanceOf[Array[AnyRef]], size + (if (insertSlotIsLast) 0 else 1), _versions.length, null)
      dumpCount
    } else {
      0
    }
  }
}
