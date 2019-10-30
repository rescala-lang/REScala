package rescala.fullmv

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.ForkJoinPool.ManagedBlocker

import rescala.core.Initializer.InitValues
import rescala.fullmv.NodeVersionHistory._

import scala.annotation.elidable.ASSERTION
import scala.annotation.{elidable, tailrec}
import scala.collection.mutable.ArrayBuffer

sealed trait FramingBranchResult[+T, +R]
object FramingBranchResult {
  case object FramingBranchEnd extends FramingBranchResult[Nothing, Nothing]
  case class Frame[T, R](out: Set[R], frame: T) extends FramingBranchResult[T, R]
  case class FrameSupersede[T, R](out: Set[R], frame: T, supersede: T) extends FramingBranchResult[T, R]
}

sealed trait NotificationResultAction[+T, +R]
object NotificationResultAction {
  // upon notify:
  //    branch merge: T/F
  //    reev: wait/ready/unchanged/unchanged+FF/unchanged+next
  // upon reevOut:
  //    done/FF/next
  case object DoNothing extends NotificationResultAction[Nothing, Nothing]
  case object ReevaluationReady extends NotificationResultAction[Nothing, Nothing]
  sealed trait NotificationOutAndSuccessorOperation[+T, R] extends NotificationResultAction[T, R] {
    val out: Set[R]
  }
  object NotificationOutAndSuccessorOperation {
    case class PureNotifyOnly[R](out: Set[R]) extends NotificationOutAndSuccessorOperation[Nothing, R]
    case class NotifyAndNonReadySuccessor[T, R](out: Set[R], succTxn: T) extends NotificationOutAndSuccessorOperation[T, R]
    case class NotifyAndReevaluationReadySuccessor[T, R](out: Set[R], succTxn: T) extends NotificationOutAndSuccessorOperation[T, R]
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
class NodeVersionHistory[V, T <: FullMVTurn, InDep, OutDep](init: T, val valuePersistency: InitValues[V]) extends FullMVState[V, T, InDep, OutDep] {
  override val host: FullMVEngine = init.host
  class Version(val txn: T, @volatile var lastWrittenPredecessorIfStable: Version, var pending: Int, var changed: Int, @volatile var value: Option[V]) extends ManagedBlocker {
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
    def isStable: Boolean = lastWrittenPredecessorIfStable != null
    // should only be used if txn >= Executing, as it may falsely return true in the case that a txn == Framing
    // had a frame converted into a marker due to frame superseding (or drop retrofitting?) and that frame was
    // marked stable after all preceding placeholders were removed but anoter txn2 == Framing inserts another
    // preceding frame which destabilizes this version again.
    def isFinal: Boolean = isWritten || (isReadOrDynamic && isStable)

    def read(): V = {
      assert(isWritten, "reading un-written "+this)
      value.get
    }

    /*@volatile*/ var finalWaiters: Int = 0
    /*@volatile*/ var stableWaiters: Int = 0

    override def block(): Boolean = NodeVersionHistory.this.synchronized {
      finalWaiters += 1
      if(!isReleasable) {
        if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] parking for final $this.")
//        LockSupport.park(NodeVersionHistory.this)
        NodeVersionHistory.this.wait()
        if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] unparked on $this.")
      }
      finalWaiters -= 1
      isReleasable
    }

    override def isReleasable: Boolean = NodeVersionHistory.this.synchronized { isFinal }

    // common blocking case (now, dynamicDepend): Use self as blocker instance to reduce garbage
    def blockForFinal: ManagedBlocker = this

    // less common blocking case
    // fake lazy val without synchronization, because it is accessed only while the node's monitor is being held.
    private var _blockForStable: ManagedBlocker = null
    def blockForStable: ManagedBlocker = {
      if(_blockForStable == null) {
        _blockForStable = new ManagedBlocker {
          override def block(): Boolean = NodeVersionHistory.this.synchronized {
        stableWaiters += 1
        while (!isStable) {
          if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] parking for stable ${Version.this}")
//          LockSupport.park(NodeVersionHistory.this)
          NodeVersionHistory.this.wait()
          if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] unparked on ${Version.this}")
        }
        stableWaiters -= 1
            isReleasable
          }
          override def isReleasable: Boolean = NodeVersionHistory.this.synchronized { isStable }
        }
      }
      _blockForStable
    }

    override def toString: String = {
      if(isWritten){
        s"Written($txn, out=$out, v=${value.get})"
      } else if (isReadOrDynamic) {
        (if(isStable) "Stable" else "Unstable") + s"Marker($txn, out=$out)"
      } else if (isOvertakeCompensation) {
        s"OvertakeCompensation($txn, ${if (isStable) "stable" else "unstable"}, out=$out, pending=$pending, changed=$changed)"
      } else if(isFrame) {
        if(isStable) {
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
        "UnknownVersionCase!(" + txn + ", " + (if(isStable) "stable" else "unstable") + ", out=" + out + ", pending=" + pending + ", changed=" + changed + ", value = " + value + ")"
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
    assert(!_versions.take(size).groupBy(_.txn).exists(_._2.length > 1), debugStatement("multiple versions for some transactions"))
    assert(indexedVersions.size == size, debugStatement(s"index $indexedVersions wrong size ${indexedVersions.size}"))

    assert(firstFrame > 0, debugStatement("firstFrame out of bounds negative"))
    assert(firstFrame <= size, debugStatement("firstFrame out of bounds positive"))
    assert(firstFrame == size || _versions(firstFrame).isFrame || _versions(firstFrame).isOvertakeCompensation, debugStatement("firstFrame not frame"))
    assert(!_versions.take(firstFrame).exists(v => v.isFrame || v.isOvertakeCompensation), debugStatement("firstFrame not first"))

    assert(latestReevOut >= 0, debugStatement("latest reevout out of bounds negative"))
    assert(latestReevOut < size, debugStatement("latestWritten out of bounds positive"))
    assert(_versions(latestReevOut).pending == 0 && _versions(latestReevOut).changed == 0 && (latestReevOut == 0 || _versions(latestReevOut).isStable), debugStatement("latestReevOut points to invalid version"))

    assert(!_versions.exists(v => v != null && v.pending == 0 && v.changed < 0), debugStatement("there's a version with pending==0 but changed<0, figure out how this comes to be and refine any isOvertakeCompensation usages or equivalents!"))

    val actualVersions = _versions.take(size)
    val expectedPredecessorWrites: Array[Version] = new Array(actualVersions.length)
    for(i <- 1 to math.min(firstFrame, size - 1)) {
      expectedPredecessorWrites(i) = if(actualVersions(i-1).isWritten) actualVersions(i-1) else expectedPredecessorWrites(i-1)
    }
    val actualPredecessorWrites = actualVersions.map(_.lastWrittenPredecessorIfStable)
    assert(expectedPredecessorWrites sameElements actualPredecessorWrites, debugStatement("broken version stability (expecteds,actuals):\n\t" + expectedPredecessorWrites.zip(actualPredecessorWrites).mkString("\n\t") + "\n" ))

    assert(latestGChint >= 0, debugStatement("latestGChint out of bounds negative"))
    assert(!_versions.take(latestGChint).exists(_.txn.phase != TurnPhase.Completed), debugStatement("latestGChint has incomplete predecessor transaction"))
    for((first,second) <- _versions.zip(_versions.tail) if first != null && second != null) {
      assert(second.txn.isTransitivePredecessor(first.txn) || first.txn.phase == TurnPhase.Completed, debugStatement(s"${first.txn} not a predecessor of ${second.txn} but both have version ordered this way"))
      assert(!first.txn.isTransitivePredecessor(second.txn), debugStatement(s"${first.txn} has predecessor cycle with ${second.txn}"))
    }
  }

  override def toString: String = super.toString + s" -> size $size, latestReevOut $latestReevOut, ${if(firstFrame == size) "no frames" else "firstFrame "+firstFrame}, latestGChint $latestGChint): \n  " + _versions.zipWithIndex.map{case (version, index) => s"$index: $version"}.mkString("\n  ")

  // =================== STORAGE ====================

  val indexedVersions = new ConcurrentHashMap[T, Version]()
  var _versions = new Array[Version](11)
  _versions(0) = new Version(init, lastWrittenPredecessorIfStable = null, pending = 0, changed = 0, Some(valuePersistency.initialValue))
  indexedVersions.put(init, _versions(0))
  var size = 1
  var latestValue: V = valuePersistency.initialValue
  var out: Set[OutDep] = Set.empty

  private def createVersionInHole(position: Int, txn: T) = {
    assert(position > 0, s"must not create version at $position <= 0")
    val predVersion = _versions(position - 1)
    val lastWrittenPredecessorIfStable = computeSuccessorWrittenPredecessorIfStable(predVersion)
    val version = new Version(txn, lastWrittenPredecessorIfStable, pending = 0, changed = 0, None)
    indexedVersions.put(txn, version)
    size += 1
    _versions(position) = version
    if (position <= firstFrame) firstFrame += 1
    if (position <= latestReevOut) latestReevOut += 1
    version
  }

  private def computeSuccessorWrittenPredecessorIfStable(predVersion: Version) = {
    if (predVersion.pending != 0 || predVersion.changed > 0) {
      null
    } else if (predVersion.isWritten) {
      predVersion
    } else {
      predVersion.lastWrittenPredecessorIfStable
    }
  }

  // =================== NAVIGATION ====================
  var firstFrame: Int = size
  var latestReevOut: Int = 0
  var latestGChint: Int = 0
  var lastGCcount: Int = 0

  val DEFAULT_MIN_POS = 0
  /**
    * determine the position or insertion point for a framing transaction
    *
    * @param txn the transaction
    * @return the position (positive values) or insertion point (negative values)
    */
  private def getFramePositionFraming(txn: T, minPos: Int = DEFAULT_MIN_POS): Int = {
    assert(minPos == DEFAULT_MIN_POS || minPos > math.max(latestGChint, latestReevOut), s"nonsensical minpos $minPos <= max(latestGChint $latestGChint, latestReevOut $latestReevOut)")
    val knownOrderedMinPosIsProvided = minPos != DEFAULT_MIN_POS
    val fromFinal = if (knownOrderedMinPosIsProvided) minPos else math.max(latestGChint, latestReevOut) + 1
    if(fromFinal == size) {
      ensureFromFinalRelationIsRecorded(size, txn, UnlockedUnknown).unlockedIfLocked()
      arrangeVersionArrayAndCreateVersion(size, txn)
    } else {
      val lastTxn = _versions(size - 1).txn
      if(lastTxn == txn) {
        // shortcut2: last version belongs to one
        lastGCcount = 0
        size - 1
      } else {
        val (success, lock) = tryRecordRelationship(lastTxn, size - 1,  txn, lastTxn, txn, UnlockedUnknown)
        val initialSCCState = lock.unlockedIfLocked()
        if (success == Succeeded) {
          // shortcut3: one could simply be ordered to the end
          arrangeVersionArrayAndCreateVersion(size, txn)
        } else {
          val (insertOrFound, _) = findOrPigeonHoleFramingPredictive(txn, fromFinal, knownOrderedMinPosIsProvided, size - 1, initialSCCState)
          if (insertOrFound < 0) {
            arrangeVersionArrayAndCreateVersion(-insertOrFound, txn)
          } else {
            lastGCcount = 0
            insertOrFound
          }
        }
      }
    }
  }

  private def ensureFromFinalRelationIsRecorded(fromFinal: Int, txn: T, sccState: SCCState): SCCState = {
    val predPos = fromFinal - 1
    val predToRecord = _versions(predPos).txn
    assert(!predToRecord.isTransitivePredecessor(txn), s"$predToRecord was concurrently ordered after $txn although we assumed this to be impossible")
    ensureRelationIsRecorded(predToRecord, predPos, txn, predToRecord, txn, sccState)
  }

  private def getFramePositionsFraming(one: T, two: T): (Int, Int) = {
    val res@(pOne, pTwo) = getFramePositionsFraming0(one, two)
    assert(_versions(pOne).txn == one, s"first position $pOne doesn't correspond to first transaction $one in $this")
    assert(_versions(pTwo).txn == two, s"second position $pTwo doesn't correspond to second transaction $two in $this")

    assert(_versions(pOne - 1).txn.phase == TurnPhase.Completed || one.isTransitivePredecessor(_versions(pOne - 1).txn), s"first $one isn't ordered after its predecessor ${_versions(pOne - 1).txn} in $this")
    assert(_versions(pOne + 1).txn.isTransitivePredecessor(one), s"first $one isn't ordered before its successor ${_versions(pOne + 1).txn}")
    assert(_versions(pTwo - 1).txn.phase == TurnPhase.Completed || two.isTransitivePredecessor(_versions(pTwo - 1).txn), s"second $two isn't ordered after its predecessor ${_versions(pTwo - 1).txn}")
    assert(pTwo + 1 == size || _versions(pTwo + 1).txn.isTransitivePredecessor(two), s"second $two isn't ordered before its successor ${_versions(pTwo + 1).txn}")
    res
  }

  private def getFramePositionsFraming0(one: T, two: T): (Int, Int) = {
    val fromFinal = math.max(latestGChint, latestReevOut) + 1
    if(fromFinal == size) {
      // shortcut1: insertion at the end is the only possible solution
      ensureFromFinalRelationIsRecorded(size, one, UnlockedUnknown).unlockedIfLocked()
      arrangeVersionArrayAndCreateVersions(size, one, size, two)
    } else {
      val lastPos = size - 1
      val lastTxn = _versions(lastPos).txn
      if(lastTxn == one) {
        // shortcut2: last version belongs to one
        arrangeVersionArrayAndCreateVersion(size, two)
        (size - 2, size - 1) // Warning: lastPos no longer valid here!
      } else {
        val (success, lock) = tryRecordRelationship(lastTxn, lastPos, one, lastTxn, one, UnlockedUnknown)
        val initialSCCState = lock.unlockedIfLocked()
        if (success == Succeeded) {
          // shortcut3: one could simply be ordered to the end
          arrangeVersionArrayAndCreateVersions(size, one, size, two)
        } else {
          val (insertOrFoundOne, sccState) = findOrPigeonHoleFramingPredictive(one, fromFinal, fromFinalPredecessorRelationIsRecorded = false, lastPos, initialSCCState)
          if(insertOrFoundOne >= 0) {
            // first one found: defer to just look for the second alone
            val insertOrFoundTwo = getFramePositionFraming(two, insertOrFoundOne + 1)
            (insertOrFoundOne - lastGCcount, insertOrFoundTwo)
          } else {
            // first one not found:
            val insertOne = -insertOrFoundOne
            if (insertOne == size) {
              arrangeVersionArrayAndCreateVersions(size, one, size, two)
            } else {
              val (insertOrFoundTwo, _) = findOrPigeonHoleFramingPredictive(two, insertOne, fromFinalPredecessorRelationIsRecorded = true, size, sccState)
              if (insertOrFoundTwo >= 0) {
                val first = arrangeVersionArrayAndCreateVersion(insertOne, one)
                (first, insertOrFoundTwo - lastGCcount + 1)
              } else {
                arrangeVersionArrayAndCreateVersions(insertOne, one, -insertOrFoundTwo, two)
              }
            }
          }
        }
      }
    }
  }

  private def getFramePositionPropagating(txn: T, minPos: Int = firstFrame): Int = {
    assert(minPos >= firstFrame, s"nonsensical minpos $minPos < firstFrame in $this")
    assert(firstFrame < size, s"propagating $txn may not have a version when looking for a frame, but there must be *some* frame in $this")
    if(minPos == size) {
      assert(txn.isTransitivePredecessor(_versions(minPos - 1).txn), s"knownOrderedMinPos $minPos for $txn: predecessor ${_versions(minPos - 1).txn} not ordered in $this")
      arrangeVersionArrayAndCreateVersion(minPos, txn)
    } else if (_versions(minPos).txn == txn) {
      // common-case shortcut attempt: receive notification for firstFrame
      lastGCcount = 0
      minPos
    } else {
      assert(minPos == firstFrame || txn.isTransitivePredecessor(_versions(minPos - 1).txn), s"minPos $minPos was given for $txn, which should have the predecessor version's ${_versions(minPos - 1).txn} as predecessor transaction, but had not in $this")
      assert(minPos > firstFrame || txn.isTransitivePredecessor(_versions(firstFrame).txn), s"propagating $txn at minPos $minPos assumes it has a frame but is not ordered after the firstFrame ${_versions(firstFrame).txn} in $this")
      val (insertOrFound, _) = findOrPigeonHolePropagatingPredictive(txn, minPos, fromFinalPredecessorRelationIsRecorded = true, size, toFinalRelationIsRecorded = true, UnlockedUnknown)
      if (insertOrFound < 0) {
        arrangeVersionArrayAndCreateVersion(-insertOrFound, txn)
      } else {
        lastGCcount = 0
        insertOrFound
      }
    }
  }

  private def getFramePositionsPropagating(one: T, two: T): (Int, Int) = {
    assert(firstFrame < size, s"a propagating turn may not have a version when looking for a frame, but there must be *some* frame.")
    if (_versions(firstFrame).txn == one) {
      // common-case shortcut attempt: receive notification for firstFrame
      val foundOne = firstFrame
      val insertOrFoundTwo = getFramePositionPropagating(two, firstFrame + 1)
      (foundOne - lastGCcount, insertOrFoundTwo)
    } else {
      val (insertOrFoundOne, sccState) = findOrPigeonHolePropagatingPredictive(one, firstFrame, fromFinalPredecessorRelationIsRecorded = false, size, toFinalRelationIsRecorded = true, UnlockedUnknown)
      if(insertOrFoundOne >= 0) {
        // first one found: defer to just look for the second alone
        val insertOrFoundTwo = getFramePositionPropagating(two, insertOrFoundOne + 1)
        (insertOrFoundOne - lastGCcount, insertOrFoundTwo)
      } else {
        // first one not found:
        val insertOne = -insertOrFoundOne
        if (insertOne == size) {
          arrangeVersionArrayAndCreateVersions(size, one, size, two)
        } else {
          val (insertOrFoundTwo, _) = findOrPigeonHolePropagatingPredictive(two, insertOne, fromFinalPredecessorRelationIsRecorded = true, size, toFinalRelationIsRecorded = true, sccState)
          if (insertOrFoundTwo >= 0) {
            val first = arrangeVersionArrayAndCreateVersion(insertOne, one)
            (first, insertOrFoundTwo - lastGCcount + 1)
          } else {
            arrangeVersionArrayAndCreateVersions(insertOne, one, -insertOrFoundTwo, two)
          }
        }
      }
    }
  }

  private def findOrPigeonHoleFramingPredictive(lookFor: T, fromFinal: Int, fromFinalPredecessorRelationIsRecorded: Boolean, toFinal: Int, sccState: SCCConnectivity): (Int, SCCConnectivity) = {
    assert(fromFinal <= size, s"binary search started with backwards indices from $fromFinal to $size")
    assert(size <= size, s"binary search upper bound $size beyond history size $size")
    assert(!_versions(fromFinal - 1).txn.isTransitivePredecessor(lookFor), s"from - 1 = ${fromFinal - 1} predecessor for non-blocking search of $lookFor pointed to version of ${_versions(fromFinal - 1).txn} which is already ordered later.")
    assert(fromFinal > 0, s"binary search started with non-positive lower bound $fromFinal")

    val r = findOrPigeonHoleFramingPredictive0(lookFor, fromFinal, fromFinalPredecessorRelationIsRecorded, fromFinal, toFinal, sccState)
    @inline def posOrInsert = r._1

    assert(_versions(latestGChint).txn.phase == TurnPhase.Completed, s"binary search returned $posOrInsert for $lookFor with GC hint $latestGChint pointing to a non-completed transaction in $this")
    assert(latestGChint < math.abs(posOrInsert), s"binary search returned $posOrInsert for $lookFor inside garbage collected section (< $latestGChint) in $this")

    assert(posOrInsert < size, s"binary search returned found at $posOrInsert for $lookFor, which is out of bounds in $this")
    assert(posOrInsert < 0 || _versions(posOrInsert).txn == lookFor, s"binary search returned found at $posOrInsert for $lookFor, which is wrong in $this")
    assert(posOrInsert >= 0 || -posOrInsert <= size, s"binary search returned insert at ${-posOrInsert}, which is out of bounds in $this")
    assert(posOrInsert >= 0 || lookFor.isTransitivePredecessor(_versions(-posOrInsert - 1).txn) || _versions(-posOrInsert - 1).txn.phase == TurnPhase.Completed, s"binary search returned insert at ${-posOrInsert} for $lookFor, but predecessor neither ordered first nor completed in $this")
    assert(posOrInsert >= 0 || -posOrInsert == size || _versions(-posOrInsert).txn.isTransitivePredecessor(lookFor), s"binary search returned insert at ${-posOrInsert} for $lookFor, but it isn't ordered before successor in $this")
    assert(posOrInsert >= 0 || -posOrInsert == size || _versions(-posOrInsert).txn.phase == TurnPhase.Framing, s"binary search returned insert at ${-posOrInsert} for $lookFor, but successor has already passed framing in $this")

    r
  }

  @tailrec
  private def findOrPigeonHoleFramingPredictive0(lookFor: T, fromFinal: Int, fromFinalPredecessorRelationIsRecorded: Boolean, fromSpeculative: Int, toFinal: Int, sccState: SCCConnectivity): (Int, SCCConnectivity) = {
    if(fromSpeculative == toFinal) {
      val (fromOrderedSuccessfully, changedSCCState) = tryOrderFromFraming(lookFor, fromFinal, fromFinalPredecessorRelationIsRecorded, fromSpeculative, sccState)
      val unlocked = changedSCCState.unlockedIfLocked()
      if(fromOrderedSuccessfully == Succeeded) {
        (-fromSpeculative, unlocked)
      } else {
        assert(unlocked == UnlockedSameSCC, s"establishing from relationship failed, but $lookFor is supposedly not in same SCC")
        findOrPigeonHoleFramingPredictive0(lookFor, fromFinal, fromFinalPredecessorRelationIsRecorded, fromFinal - 1, fromSpeculative, UnlockedSameSCC)
      }
    } else {
      val probe = fromSpeculative+(toFinal-fromSpeculative-1)/2
      val candidate = _versions(probe).txn
      if(candidate == lookFor) {
        (probe, sccState.known)
      } else {
        candidate.phase match {
          case TurnPhase.Completed =>
            latestGChint = probe
            findOrPigeonHoleFramingPredictive0(lookFor, probe + 1, fromFinalPredecessorRelationIsRecorded = true, probe + 1, toFinal, sccState)
          case TurnPhase.Executing =>
            assert(!candidate.isTransitivePredecessor(lookFor), s"framing $lookFor should not be predecessor of some executing $candidate")
            findOrPigeonHoleFramingPredictive0(lookFor, probe + 1, fromFinalPredecessorRelationIsRecorded = false, probe + 1, toFinal, sccState)
          case TurnPhase.Framing =>
            if (lookFor.isTransitivePredecessor(candidate)) {
              findOrPigeonHoleFramingPredictive0(lookFor, probe + 1, fromFinalPredecessorRelationIsRecorded = true, probe + 1, toFinal, sccState.known)
            } else if (candidate.isTransitivePredecessor(lookFor)) {
              findOrPigeonHoleFramingPredictive0(lookFor, fromFinal, fromFinalPredecessorRelationIsRecorded, fromSpeculative, probe, sccState.known)
            } else {
              findOrPigeonHoleFramingPredictive0(lookFor, fromFinal, fromFinalPredecessorRelationIsRecorded, probe + 1, toFinal, sccState)
            }
          case unknown =>
            throw new AssertionError(s"$candidate has unknown phase $unknown")
        }
      }
    }
  }

  private def findOrPigeonHolePropagatingPredictive(lookFor: T, fromFinal: Int, fromFinalPredecessorRelationIsRecorded: Boolean, toFinal: Int, toFinalRelationIsRecorded: Boolean, sccState: SCCConnectivity): (Int, SCCConnectivity) = {
    assert(fromFinal <= toFinal, s"binary search started with backwards indices from $fromFinal to $toFinal")
    assert(toFinal <= size, s"binary search upper bound $toFinal beyond history size $size")
    // the following assert possibly requires an additional || _versions(toFinal).txn == lookFor
    assert(toFinal == size || !(lookFor.isTransitivePredecessor(_versions(toFinal).txn) || _versions(toFinal).txn.phase == TurnPhase.Completed), s"to = $toFinal successor for non-blocking search of known static $lookFor pointed to version of ${_versions(toFinal).txn} which is ordered earlier in $this")
    assert(!_versions(fromFinal - 1).txn.isTransitivePredecessor(lookFor), s"from - 1 = ${fromFinal - 1} predecessor for non-blocking search of $lookFor pointed to version of ${_versions(fromFinal - 1).txn} which is already ordered later in $this")
    assert(fromFinal > 0, s"binary search started with non-positive lower bound $fromFinal")

    val r = findOrPigeonHolePropagatingPredictive0(lookFor, fromFinal, fromFinalPredecessorRelationIsRecorded, fromFinal, toFinal, toFinal, toFinalRelationIsRecorded, sccState)
    @inline def posOrInsert = r._1

    assert(_versions(latestGChint).txn.phase == TurnPhase.Completed, s"binary search returned $posOrInsert for $lookFor with GC hint $latestGChint pointing to a non-completed transaction in $this")
    assert(latestGChint < math.abs(posOrInsert), s"binary search returned $posOrInsert for $lookFor inside garbage collected section (< $latestGChint) in $this")

    assert(posOrInsert < size, s"binary search returned found at $posOrInsert for $lookFor, which is out of bounds in $this")
    assert(posOrInsert < 0 || _versions(posOrInsert).txn == lookFor, s"binary search returned found at $posOrInsert for $lookFor, which is wrong in $this")
    assert(posOrInsert >= 0 || -posOrInsert <= size, s"binary search returned insert at ${-posOrInsert}, which is out of bounds in $this")
    assert(posOrInsert >= 0 || lookFor.isTransitivePredecessor(_versions(-posOrInsert - 1).txn) || _versions(-posOrInsert - 1).txn.phase == TurnPhase.Completed, s"binary search returned insert at ${-posOrInsert} for $lookFor, but predecessor isn't ordered first in $this")
    assert(posOrInsert >= 0 || -posOrInsert == toFinal || _versions(-posOrInsert).txn.isTransitivePredecessor(lookFor), s"binary search returned insert at ${-posOrInsert} for $lookFor, but it isn't ordered before successor in $this")

    r
  }

  @tailrec
  private def findOrPigeonHolePropagatingPredictive0(lookFor: T, fromFinal: Int, fromFinalPredecessorRelationIsRecorded: Boolean, fromSpeculative: Int, toSpeculative: Int, toFinal: Int, toFinalRelationIsRecorded: Boolean, sccState: SCCConnectivity): (Int, SCCConnectivity) = {
    if(fromSpeculative == toSpeculative) {
      val (fromOrderedResult, asdasd) = tryOrderFromPropagating(lookFor, fromFinal, fromFinalPredecessorRelationIsRecorded, fromSpeculative, sccState)
      val changedSCCState = asdasd.unlockedIfLocked() // TODO i don't know why not unlocking here leads to deadlocks...
      fromOrderedResult match {
        case Succeeded =>
          val (toOrderedSuccessfully, againChangedSCCState) = tryOrderToPropagating(lookFor, toSpeculative, toFinal, toFinalRelationIsRecorded, changedSCCState)
          val unlocked = againChangedSCCState.unlockedIfLocked()
          if(toOrderedSuccessfully == Succeeded) {
            (-fromSpeculative, unlocked.known)
          } else {
            findOrPigeonHolePropagatingPredictive0(lookFor, toSpeculative, fromFinalPredecessorRelationIsRecorded = true, toSpeculative, toFinal, toFinal, toFinalRelationIsRecorded, unlocked)
          }
        case FailedFinalAndRecorded =>
          val unlocked = changedSCCState.unlockedIfLocked()
          findOrPigeonHolePropagatingPredictive0(lookFor, fromFinal, fromFinalPredecessorRelationIsRecorded, fromFinal, fromSpeculative - 1, fromSpeculative - 1, toFinalRelationIsRecorded = true, unlocked)
        case FailedNonfinal =>
          // This case is a bit of a weird one. The following events occurred:
          // an operation such as notifyFollowFrame(X, lookFor) was executed.
          // It searched for the position of lookFor, which at that time was Framing.
          // It encountered a version of Y, which was also framing, and took it as lower bound.
          // Concurrently, lookFor transitioned to Executing
          // Then, the current termination attempt occurred.
          // It attempted to record Y < lookFor, but that failed because Y is still Framing but lookFor now Executing, and thus must go earlier.
          // Now we are here.
          // Now because lookFor must go before Y, we use the position of Y as the new exclusive upper bound for the fallback search.
          // In the future, though, Y may also transition to Executing.
          // Once that happened, a different Task also involving lookFor may concurrently establish the originally attempted order of Y < lookFor.
          // Thus, we must keep the previous toFinal bound, and can use Y only for speculation.
          // (Lastly, for concerned readers, this loop terminates because when the fallback search itself then fails, it uses Y as new _final_ lower Bound, so the final range is guaranteed to shrink before repeating.)
          val unlocked = changedSCCState.unlockedIfLocked()
          findOrPigeonHolePropagatingPredictive0(lookFor, fromFinal, fromFinalPredecessorRelationIsRecorded, fromFinal, fromSpeculative - 1, toFinal, toFinalRelationIsRecorded, unlocked)
      }
    } else {
      val probe = fromSpeculative+(toSpeculative-fromSpeculative-1)/2
      val candidate = _versions(probe).txn
      if(candidate == lookFor) {
        (probe, sccState)
      } else {
        candidate.phase match {
          case TurnPhase.Completed =>
            latestGChint = probe
            findOrPigeonHolePropagatingPredictive0(lookFor, probe + 1, fromFinalPredecessorRelationIsRecorded = true, probe + 1, toSpeculative, toFinal, toFinalRelationIsRecorded, sccState)
          case otherwise =>
            if (lookFor.isTransitivePredecessor(candidate)) {
              findOrPigeonHolePropagatingPredictive0(lookFor, probe + 1, fromFinalPredecessorRelationIsRecorded = true, probe + 1, toSpeculative, toFinal, toFinalRelationIsRecorded, sccState.known)
            } else if (candidate.isTransitivePredecessor(lookFor)) {
              findOrPigeonHolePropagatingPredictive0(lookFor, fromFinal, fromFinalPredecessorRelationIsRecorded, fromSpeculative, probe, probe, toFinalRelationIsRecorded = true, sccState.known)
            } else if(lookFor.phase <= otherwise) {
              findOrPigeonHolePropagatingPredictive0(lookFor, fromFinal, fromFinalPredecessorRelationIsRecorded, probe + 1, toSpeculative, toFinal, toFinalRelationIsRecorded, sccState)
            } else {
              findOrPigeonHolePropagatingPredictive0(lookFor, fromFinal, fromFinalPredecessorRelationIsRecorded, fromSpeculative, probe, toFinal, toFinalRelationIsRecorded, sccState)
            }
        }
      }
    }
  }

  // there is no tryOrderToFraming because framing turns always try to order themselves at the end
  private def tryOrderFromFraming(lookFor: T, fromFinal: Int, fromFinalPredecessorRelationIsRecorded: Boolean, fromSpeculative: Int, sccState: SCCState): (TryRecordResult, SCCState) = {
    if (fromSpeculative > fromFinal) {
      val predPos = fromSpeculative - 1
      val predToRecord = _versions(predPos).txn
      tryRecordRelationship(predToRecord, predPos, lookFor, predToRecord, lookFor, sccState)
    } else if (!fromFinalPredecessorRelationIsRecorded) {
      assert(fromFinal == fromSpeculative, s"someone speculated fromSpeculative=$fromSpeculative smaller than fromFinal=$fromFinal")
      (Succeeded, ensureFromFinalRelationIsRecorded(fromFinal, lookFor, sccState))
    } else {
      // there is no from to order
      (Succeeded, sccState)
    }
  }

  private def tryOrderFromPropagating(lookFor: T, fromFinal: Int, fromFinalPredecessorRelationIsRecorded: Boolean, fromSpeculative: Int, sccState: SCCState): (TryOrderResult, SCCState) = {
    if (fromSpeculative > fromFinal) {
      val predPos = fromSpeculative - 1
      val predToRecord = _versions(predPos).txn
      predToRecord.phase match {
        case TurnPhase.Completed =>
          latestGChint = predPos
          // last chance to skip recording effort if predecessor completed concurrently
          (Succeeded, sccState)
        case TurnPhase.Executing =>
          tryRecordRelationship(predToRecord, predPos, lookFor, predToRecord, lookFor, sccState)
        case TurnPhase.Framing =>
          FullMVEngine.myAwait(lookFor.acquireRemoteBranchIfPhaseAtMost(TurnPhase.Framing), host.timeout) match {
            case TurnPhase.Framing =>
              try {
                tryRecordRelationship(predToRecord, predPos, lookFor, predToRecord, lookFor, sccState)
              } finally {
                lookFor.asyncRemoteBranchComplete(TurnPhase.Framing)
              }
            case TurnPhase.Executing =>
              // race conflict: lookFor was Framing earlier and ordered itself behind predToRecord, but lookFor
              (FailedNonfinal, sccState)
            case TurnPhase.Completed => throw new AssertionError(s"lookFor should not be able to complete concurrently")
            case unknown => throw new AssertionError(s"$lookFor unknown phase $unknown")
          }
        case unknown => throw new AssertionError(s"$predToRecord unknown phase $unknown")
      }
    } else if (!fromFinalPredecessorRelationIsRecorded) {
      assert(fromFinal == fromSpeculative, s"someone speculated fromSpeculative=$fromSpeculative smaller than fromFinal=$fromFinal")
      (Succeeded, ensureFromFinalRelationIsRecorded(fromFinal, lookFor, sccState))
    } else {
      // there is no from to order
      (Succeeded, sccState)
    }
  }

  private def tryOrderToPropagating(lookFor: T, toSpeculative: Int, toFinal: Int, toFinalRelationIsRecorded: Boolean, sccState: SCCState): (TryRecordResult, SCCState) = {
    if (toSpeculative < toFinal) {
      assert(lookFor.phase == TurnPhase.Executing, s"$lookFor has a speculative successor, which should only happen if it is no longer framing.")
      val succToRecord = _versions(toSpeculative).txn
      FullMVEngine.myAwait(succToRecord.acquireRemoteBranchIfPhaseAtMost(TurnPhase.Executing), host.timeout) match {
        case TurnPhase.Completed =>
          latestGChint = toSpeculative
          (Succeeded, sccState)
        case TurnPhase.Executing =>
          // could also acquirePhaseLockIfAtMost(TurnPhase.Framing) and default this case to (false, sccState), since
          // succToRecord must have been framing to have been made toSpeculative earlier, but now clearly isn't anymore
          // and thus the previous decision is no longer valid.
          // this might in turn allow simplifications for turn phase switching as only the transition framing->executing
          // may have new predecessors pushed before it concurrently, but once a turn is executing, only his own thread
          // would be modifying its predecessors.
          try {
            tryRecordRelationship(lookFor, -1, succToRecord, succToRecord, lookFor, sccState)
          } finally {
            succToRecord.asyncRemoteBranchComplete(TurnPhase.Executing)
          }
        case TurnPhase.Framing =>
          try {
            val lock = ensureRelationIsRecorded(lookFor, -1, succToRecord, succToRecord, lookFor, sccState)
            (Succeeded, lock)
          } finally {
            succToRecord.asyncRemoteBranchComplete(TurnPhase.Framing)
          }
        case unknown =>
          throw new AssertionError(s"$succToRecord has unknown phase $unknown")
      }
    } else if (!toFinalRelationIsRecorded) {
      assert(toFinal == toSpeculative, s"someone speculated toSpeculative=$toSpeculative as larger than toFinal=$toFinal")
      assert(lookFor.phase == TurnPhase.Executing, s"$lookFor should only have a final but unrecorded to-relation if does a static read, i.e., is executing.")
      val succToRecord = _versions(toFinal).txn
      // while this assertion held when toFinal was decided, it may no longer be valid here, e.g. for a follow framing, which may have switched to Executing since then
      // assert(succToRecord.phase == TurnPhase.Framing, s"$succToRecord should only be a final but unrecorded to-relation that has framed a node, but not the static successor where $lookFor came from and thus not completed framing")
      val lock = ensureRelationIsRecorded(lookFor, -1, succToRecord, succToRecord, lookFor, sccState)
      (Succeeded, lock)
    } else {
      // there is no to to order
      (Succeeded, sccState)
    }
  }

  private def tryRecordRelationship(attemptPredecessor: T, predPos: Int, succToRecord: T, defender: T, contender: T, sccState: SCCState): (TryRecordResult, SCCState) = {
    //    @inline @tailrec def tryRecordRelationship0(sccState: SCCState): (TryRecordResult, SCCState) = {
    sccState match {
      case x: LockedSameSCC =>
        if (attemptPredecessor.isTransitivePredecessor(succToRecord)) {
          (FailedFinalAndRecorded, x)
        } else {
          ensurePredecessorRelationRecordedUnderLock(attemptPredecessor, predPos, succToRecord)
          (Succeeded, x)
        }
      case otherwise =>
        Thread.`yield`()
        if (attemptPredecessor.phase == TurnPhase.Completed) {
          assert(predPos >= 0, s"supposed-to-be predecessor $attemptPredecessor completed this having been assumed impossible")
          if(predPos < 0) throw new AssertionError(s"supposed-to-be predecessor $attemptPredecessor completed this having been assumed impossible")
          latestGChint = predPos
          //            SerializationGraphTracking.abortContending()
          (Succeeded, sccState)
        } else if(succToRecord.isTransitivePredecessor(attemptPredecessor)) {
          //            SerializationGraphTracking.abortContending()
          (Succeeded, sccState.known)
        } else if (attemptPredecessor.isTransitivePredecessor(succToRecord)) {
          //            SerializationGraphTracking.abortContending()
          (FailedFinalAndRecorded, sccState.known)
        } else {
          //            tryRecordRelationship0(SerializationGraphTracking.tryLock(defender, contender, sccState))
          tryRecordRelationship(attemptPredecessor, predPos, succToRecord, defender, contender, SerializationGraphTracking.tryLock(defender, contender, sccState))
        }
    }
    //    }
    //    sccState match {
    //      case x: LockedSameSCC =>
    //        if (attemptPredecessor.isTransitivePredecessor(succToRecord)) {
    //          (FailedFinalAndRecorded, x)
    //        } else {
    //          ensurePredecessorRelationRecordedUnderLock(attemptPredecessor, predPos, succToRecord)
    //          (Succeeded, x)
    //        }
    //      case otherwise =>
    //        if (attemptPredecessor.phase == TurnPhase.Completed) {
    //          assert(predPos >= 0, s"supposed-to-be predecessor $attemptPredecessor completed this having been assumed impossible")
    //          latestGChint = predPos
    //          (Succeeded, sccState)
    //        } else if (succToRecord.isTransitivePredecessor(attemptPredecessor)) {
    //          (Succeeded, sccState.known)
    //        } else if (attemptPredecessor.isTransitivePredecessor(succToRecord)) {
    //          (FailedFinalAndRecorded, sccState.known)
    //        } else {
    //          SerializationGraphTracking.startContending()
    //          tryRecordRelationship0(SerializationGraphTracking.tryLock(defender, contender, sccState))
    //        }
    //    }
  }

  private def ensureRelationIsRecorded(predecessor: T, predPos: Int, successor: T, defender: T, contender: T, sccState: SCCState): SCCState = {
    //    @inline @tailrec def ensureRelationIsRecorded0(sccState: SCCState): SCCState = {
    sccState match {
      case x: LockedSameSCC =>
        ensurePredecessorRelationRecordedUnderLock(predecessor, predPos, successor)
        x
      case otherwise =>
        Thread.`yield`()
        if (predecessor.phase == TurnPhase.Completed) {
          assert(predPos >= 0, s"supposed-to-be predecessor $predecessor completed this having been assumed impossible")
          if(predPos < 0) throw new AssertionError(s"supposed-to-be predecessor $predecessor completed this having been assumed impossible")
          latestGChint = predPos
          //            SerializationGraphTracking.abortContending()
          sccState
        } else if (successor.isTransitivePredecessor(predecessor)) {
          //            SerializationGraphTracking.abortContending()
          sccState.known
        } else {
          //            ensureRelationIsRecorded0(SerializationGraphTracking.tryLock(defender, contender, sccState))
          ensureRelationIsRecorded(predecessor, predPos, successor, defender, contender, SerializationGraphTracking.tryLock(defender, contender, sccState))
        }
    }
    //    }
    //    sccState match {
    //      case x: LockedSameSCC =>
    //        ensurePredecessorRelationRecordedUnderLock(predecessor, predPos, successor)
    //        x
    //      case otherwise =>
    //        if (predecessor.phase == TurnPhase.Completed) {
    //          assert(predPos >= 0, s"supposed-to-be predecessor $predecessor completed this having been assumed impossible")
    //          latestGChint = predPos
    //          sccState
    //        } else if (successor.isTransitivePredecessor(predecessor)) {
    //          sccState.known
    //        } else {
    //          SerializationGraphTracking.startContending()
    //          ensureRelationIsRecorded0(SerializationGraphTracking.tryLock(defender, contender, sccState))
    //        }
    //    }
  }

  private def ensurePredecessorRelationRecordedUnderLock(predecessor: T, predPos: Int, successor: T): Unit = {
    if (!successor.isTransitivePredecessor(predecessor)) {
      val tree = predecessor.selfNode
      if (tree == null) {
        assert(predecessor.phase == TurnPhase.Completed, s"$predecessor selfNode was null but isn't completed?")
        assert(predPos >= 0, s"supposed-to-be predecessor $predecessor completed this having been assumed impossible")
        if(predPos < 0) throw new AssertionError(s"supposed-to-be predecessor $predecessor completed this having been assumed impossible")
        latestGChint = predPos
      } else {
        FullMVEngine.myAwait(successor.addPredecessor(tree), host.timeout)
      }
    }
  }

  /**
    * determine the position or insertion point for a transaction for which
    * this node is known to have become final
    *
    * @param txn the transaction
    * @return the position (positive values) or insertion point (negative values)
    */
  private def findFinalPosition/*Propagating*/(txn: T): Int = {
    if (_versions(latestReevOut).txn == txn) {
      // common-case shortcut attempt: read latest completed reevaluation
      latestReevOut
    } else {
      val res = findOrPigeonHolePropagatingPredictive(txn, latestGChint + 1, fromFinalPredecessorRelationIsRecorded = true, firstFrame, toFinalRelationIsRecorded = firstFrame == size, UnlockedUnknown /* minor TODO might be able to have KnownSame here? */)._1
      assert(res < 0 || _versions(res).isFinal, s"found version $res of $txn isn't final in $this")
      assert(res < 0 || _versions(res).txn == txn, s"found version $res doesn't belong to $txn in $this")
      assert(res >= 0 || _versions(-res - 1).isFinal, s"predecessor version of insert point $res of $txn isn't final in $this")
      assert(res >= 0 || txn.isTransitivePredecessor(_versions(-res - 1).txn) || _versions(-res - 1).txn.phase == TurnPhase.Completed, s"predecessor of insert point ${-res} isn't ordered before $txn in $this")
      assert(res >= 0 || -res == size || _versions(-res).txn.isTransitivePredecessor(txn), s"successor of insert point ${-res} isn't ordered after $txn in $this")
      res
    }
  }


  // =================== FRAMING ====================

  /**
    * entry point for regular framing
    *
    * @param txn the transaction visiting the node for framing
    */
  override def incrementFrame(txn: T): FramingBranchResult[T, OutDep] = synchronized {
    val result = incrementFrame0(txn, getFramePositionFraming(txn))
    assertOptimizationsIntegrity(s"incrementFrame($txn) -> $result")
    result
  }

  /**
    * entry point for superseding framing
    * @param txn the transaction visiting the node for framing
    * @param supersede the transaction whose frame was superseded by the visiting transaction at the previous node
    */
  override def incrementSupersedeFrame(txn: T, supersede: T): FramingBranchResult[T, OutDep] = synchronized {
    val (position, supersedePos) = getFramePositionsFraming(txn, supersede)
    _versions(supersedePos).pending -= 1
    val result = incrementFrame0(txn, position)
    assertOptimizationsIntegrity(s"incrementSupersedeFrame($txn, $supersede) -> $result")
    result
  }

  private def incrementFrame0(txn: T, position: Int): FramingBranchResult[T, OutDep] = {
    val version = _versions(position)
    version.pending += 1
    if(position == firstFrame) {
      assert(version.pending != 1, s"previously not a frame $version was already pointed to as firstFrame in $this")
      if(version.pending == 0) {
        // if first frame was removed (i.e., overtake compensation was resolved -- these cases mirror progressToNextWriteForNotification)
        val maybeNewFirstFrame = stabilizeForwardsUntilFrame(version.lastWrittenPredecessorIfStable)
        if (maybeNewFirstFrame == null || maybeNewFirstFrame.pending < 0) {
          FramingBranchResult.FramingBranchEnd
        } else {
          FramingBranchResult.Frame(out, maybeNewFirstFrame.txn)
        }
      } else {
        // just incremented an already existing and propagated frame
        FramingBranchResult.FramingBranchEnd
      }
    } else if(position < firstFrame) {
      // created a new frame
      assert(version.pending == 1, s"found overtake or frame compensation $version before firstFrame in $this")
      val maybeSupersede = if(firstFrame < size) {
        val pffv = _versions(firstFrame)
        if(pffv.pending >= 0) {
          // technically .isFrame, i.e., > 0, but pffv may have been superseded from 1 to 0 before this call.
          pffv.txn
        } else {
          // pffv is overtake
          null.asInstanceOf[T]
        }
      } else {
        null.asInstanceOf[T]
      }
      destabilizeBackwardsUntil(position)
      assert(_versions(firstFrame) == version, s"destabilize stopped at different firstFrame than $version in $this")
      if(maybeSupersede == null) {
        FramingBranchResult.Frame(out, txn)
      } else {
        FramingBranchResult.FrameSupersede(out, txn, maybeSupersede)
      }
    } else /* if(position > firstFrame)*/  {
      // created or incremented a non-first frame
      FramingBranchResult.FramingBranchEnd
    }
  }


  @tailrec private def destabilizeBackwardsUntil(pos: Int): Unit = {
    if(firstFrame < size) {
      val destabilize = _versions(firstFrame)
      assert(destabilize.isStable, s"cannot destabilize $firstFrame: $destabilize")
      destabilize.lastWrittenPredecessorIfStable = null
    }
    firstFrame -= 1
    if(pos < firstFrame) destabilizeBackwardsUntil(pos)
  }

  @tailrec private def stabilizeForwardsUntilFrame(stabilizeTo: Version): Version = {
    val finalized = _versions(firstFrame)
    if(finalized.finalWaiters > 0) {
      //      if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] unparking ${finalized.txn.userlandThread.getName} after finalized $finalized.")
      //      LockSupport.unpark(finalized.txn.userlandThread)
      NodeVersionHistory.this.notifyAll()
    }
    firstFrame += 1
    if (firstFrame < size) {
      val stabilized = _versions(firstFrame)
      assert(!stabilized.isStable, s"cannot stabilize $firstFrame: $stabilized")
      stabilized.lastWrittenPredecessorIfStable = stabilizeTo
      if(stabilized.stableWaiters > 0) {
//        if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] unparking ${stabilized.txn.userlandThread.getName} after stabilized $stabilized.")
//        LockSupport.unpark(stabilized.txn.userlandThread)
        NodeVersionHistory.this.notifyAll()
      }
      if (stabilized.pending == 0) {
        assert(stabilized.changed >= 0, s"stablizeTo ran up to $stabilized with negative change in $this")
        if (stabilized.changed == 0) {
          stabilizeForwardsUntilFrame(stabilizeTo)
        } else {
          stabilized
        }
      } else {
        stabilized
      }
    } else {
      null
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
  override def notify(txn: T, changed: Boolean): (Boolean, NotificationResultAction[T, OutDep]) = synchronized {
    val result = notify0(getFramePositionPropagating(txn), txn, changed)
    assertOptimizationsIntegrity(s"notify($txn, $changed) -> $result")
    result
  }

  /**
    * entry point for change/nochange notification reception with follow-up framing
    * @param txn the transaction sending the notification
    * @param changed whether or not the dependency changed
    * @param followFrame a transaction for which to create a subsequent frame, furthering its partial framing.
    */
  override def notifyFollowFrame(txn: T, changed: Boolean, followFrame: T): (Boolean, NotificationResultAction[T, OutDep]) = synchronized {
    val (pos, followPos) = getFramePositionsPropagating(txn, followFrame)
    _versions(followPos).pending += 1
    val result = notify0(pos, txn, changed)
    assertOptimizationsIntegrity(s"notifyFollowFrame($txn, $changed, $followFrame) -> $result")
    result
  }

  private def notify0(position: Int, txn: T, changed: Boolean): (Boolean, NotificationResultAction[T, OutDep]) = {
    val version = _versions(position)
    // This assertion is probably pointless as it only verifies a subset of assertStabilityIsCorrect, i.e., if this
    // would fail, then assertStabilityIsCorrect will have failed at the end of the previous operation already.
    assert((position == firstFrame) == version.isStable, s"firstFrame and stable diverted in $this")

    // note: if the notification overtook a previous turn's notification with followFraming for this transaction,
    // pending may update from 0 to -1 here
    version.pending -= 1
    if (changed) {
      // note: if drop retrofitting overtook the change notification, change may update from -1 to 0 here!
      version.changed += 1
    }

    // check if the notification triggers subsequent actions
    if (position == firstFrame && version.pending == 0) {
      if (version.changed > 0) {
        (version.changed == 1, NotificationResultAction.ReevaluationReady)
      } else {
        // ResolvedFirstFrameToUnchanged
        (true, progressToNextWriteForNotification(version, version.lastWrittenPredecessorIfStable))
      }
    } else {
      (version.changed == 1, NotificationResultAction.DoNothing)
    }
  }

  override def reevIn(turn: T): V = {
    assert(synchronized {_versions(firstFrame).txn == turn }, s"$turn called reevIn, but is not first frame owner in $this")
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
    assert(version.pending == 0, s"$turn completed reevaluation, but first frame $version is no longer glitch free ready -- retrofitting during reevaluation could cause this, but dynamicAfter should be implemented to suspend for final, which should make such a retrofitting case impossible.")
    assert((version.isFrame && version.isReadyForReevaluation) || (maybeValue.isEmpty && version.isReadOrDynamic), s"$turn cannot write changed=${maybeValue.isDefined} in $this")

    version.changed = 0
    latestReevOut = position
    val stabilizeTo = if (maybeValue.isDefined) {
      latestValue = valuePersistency.unchange.unchange(maybeValue.get)
      version.value = maybeValue
      version
    } else {
      version.lastWrittenPredecessorIfStable
    }
    val result = progressToNextWriteForNotification(version, stabilizeTo)
    assertOptimizationsIntegrity(s"reevOut($turn, ${maybeValue.isDefined}) -> $result")
    result
  }

  /**
    * progresses [[firstFrame]] forward until a [[Version.isFrame]] is encountered and assemble all necessary
    * information to send out change/nochange notifications for the given transaction. Also capture synchronized,
    * whether or not the possibly encountered write [[Version.isReadyForReevaluation]].
    * @return the notification and next reevaluation descriptor.
    */
  private def progressToNextWriteForNotification(finalizedVersion: Version, stabilizeTo: Version): NotificationResultAction.NotificationOutAndSuccessorOperation[T, OutDep] = {
    val maybeNewFirstFrame = stabilizeForwardsUntilFrame(stabilizeTo)
    val res = if(maybeNewFirstFrame != null) {
      if(maybeNewFirstFrame.pending == 0) {
        assert(maybeNewFirstFrame.changed != 0, s"stabilize stopped at marker $maybeNewFirstFrame in $this")
        if(maybeNewFirstFrame.changed > 0) {
          NotificationResultAction.NotificationOutAndSuccessorOperation.NotifyAndReevaluationReadySuccessor(out, maybeNewFirstFrame.txn)
        } else /* if(maybeNewFirstFrame.changed < 0) */ {
          throw new AssertionError("need figure out how to handle this case if we find that it can occur, assuming for now it cannot...")
        }
      } else if(maybeNewFirstFrame.pending > 0) {
        NotificationResultAction.NotificationOutAndSuccessorOperation.NotifyAndNonReadySuccessor(out, maybeNewFirstFrame.txn)
      } else /* if(maybeNewFirstFrame.pending < 0) */ {
        NotificationResultAction.NotificationOutAndSuccessorOperation.PureNotifyOnly(out)
      }
    } else {
      NotificationResultAction.NotificationOutAndSuccessorOperation.PureNotifyOnly(out)
    }
    res
  }

  // =================== READ OPERATIONS ====================

  /**
    * ensures at least a read version is stored to track executed reads or dynamic operations.
    * @param txn the executing transaction
    * @return the version's position.
    */
  private def ensureReadVersion(txn: T, knownOrderedMinPos: Int = latestGChint + 1): Int = {
    assert(knownOrderedMinPos > latestGChint, s"nonsensical minpos $knownOrderedMinPos <= latestGChint $latestGChint")
    if(knownOrderedMinPos == size) {
      assert(txn.isTransitivePredecessor(_versions(knownOrderedMinPos - 1).txn) || _versions(knownOrderedMinPos - 1).txn.phase == TurnPhase.Completed, s"illegal $knownOrderedMinPos: predecessor ${_versions(knownOrderedMinPos - 1).txn} not ordered in $this")
      arrangeVersionArrayAndCreateVersion(size, txn)
    } else if (_versions(latestReevOut).txn == txn) {
      lastGCcount = 0
      latestReevOut
    } else {
      val (insertOrFound, _) = findOrPigeonHolePropagatingPredictive(txn, knownOrderedMinPos, fromFinalPredecessorRelationIsRecorded = true, size, toFinalRelationIsRecorded = true, UnlockedUnknown)
      if(insertOrFound < 0) {
        arrangeVersionArrayAndCreateVersion(-insertOrFound, txn)
      } else {
        lastGCcount = 0
        insertOrFound
      }
    }
  }

  /**
    * entry point for before(this); may suspend.
    *
    * @param txn the executing transaction
    * @return the corresponding [[Version.value]] from before this transaction, i.e., ignoring the transaction's
    *         own writes.
    */
  override def dynamicBefore(txn: T): V = {
    //    assert(!valuePersistency.isTransient, s"$txn invoked dynamicBefore on transient node")
    val maybeVersion = indexedVersions.get(txn)
    val version = if(maybeVersion != null) maybeVersion else synchronized {
      val pos = ensureReadVersion(txn)
      // DO NOT INLINE THIS! it breaks the code! see https://scastie.scala-lang.org/briJDRO3RCmIMEd1zApmBQ
      _versions(pos)
    }
    if(!version.isStable) ForkJoinPool.managedBlock(version.blockForStable)
    version.lastWrittenPredecessorIfStable.value.get
  }

  override def staticBefore(txn: T): V = {
    //    assert(!valuePersistency.isTransient, s"$txn invoked staticBefore on transient struct")
    val maybeVersion = indexedVersions.get(txn)
    val version = if(maybeVersion != null) maybeVersion else synchronized {
      val pos = findFinalPosition(txn)
      _versions(if (pos < 0) -pos - 1 else pos)
    }
    if(version.txn != txn && version.value.isDefined) {
      version.value.get
    } else {
      version.lastWrittenPredecessorIfStable.value.get
    }
  }

  /**
    * entry point for after(this); may suspend.
    * @param txn the executing transaction
    * @return the corresponding [[Version.value]] from after this transaction, i.e., awaiting and returning the
    *         transaction's own write if one has occurred or will occur.
    */
  override def dynamicAfter(txn: T): V = {
    val maybeVersion = indexedVersions.get(txn)
    val version = if(maybeVersion != null) maybeVersion else synchronized {
      val pos = ensureReadVersion(txn)
      // DO NOT INLINE THIS! it breaks the code! see https://scastie.scala-lang.org/briJDRO3RCmIMEd1zApmBQ
      _versions(pos)
    }
    if(!version.isFinal) ForkJoinPool.managedBlock(version.blockForFinal)
    if (version.value.isDefined) {
      version.value.get
    } else {
      valuePersistency.unchange.unchange(version.lastWrittenPredecessorIfStable.value.get)
    }
  }

  override def staticAfter(txn: T): V = {
    val maybeVersion = indexedVersions.get(txn)
    val version = if(maybeVersion != null) maybeVersion else synchronized {
      val pos = findFinalPosition(txn)
      _versions(if (pos < 0) -pos - 1 else pos)
    }
    if(version.value.isDefined) {
      if(version.txn == txn) {
        version.value.get
      } else {
        valuePersistency.unchange.unchange(version.value.get)
      }
    } else {
      valuePersistency.unchange.unchange(version.lastWrittenPredecessorIfStable.value.get)
    }
  }

  // =================== DYNAMIC OPERATIONS ====================

  /**
    * entry point for discover(this, add). May suspend.
    * @param txn the executing reevaluation's transaction
    * @param add the new edge's sink node
    * @return the appropriate [[Version.value]].
    */
  override def discover(txn: T, add: OutDep): (List[T], Option[T]) = synchronized {
    val position = ensureReadVersion(txn)
    assert(!out.contains(add), "must not discover an already existing edge!")
    out += add
    retrofitSourceOuts(position)
  }

  /**
    * entry point for drop(this, ticket.issuer); may suspend temporarily.
    * @param txn the executing reevaluation's transaction
    * @param remove the removed edge's sink node
    */
  override def drop(txn: T, remove: OutDep): (List[T], Option[T]) = synchronized {
    val position = ensureReadVersion(txn)
    assert(out.contains(remove), "must not drop a non-existing edge!")
    out -= remove
    retrofitSourceOuts(position)
  }

  /**
    * performs the reframings on the sink of a discover(n, this) with arity +1, or drop(n, this) with arity -1
    * @param successorWrittenVersions the reframings to perform for successor written versions
    * @param maybeSuccessorFrame maybe a reframing to perform for the first successor frame
    * @param arity +1 for discover adding frames, -1 for drop removing frames.
    */
  override def retrofitSinkFrames(successorWrittenVersions: Seq[T], maybeSuccessorFrame: Option[T], arity: Int): Seq[T]= synchronized {
    require(math.abs(arity) == 1)
    var minPos = firstFrame
    val res = successorWrittenVersions.filter { txn =>
      val position = ensureReadVersion(txn, minPos)
      val version = _versions(position)
      // note: if drop retrofitting overtook a change notification, changed may update from 0 to -1 here!
      val before = version.changed == 0
      version.changed += arity
      val after = version.changed == 0
      minPos = position + 1
      before != after
    }

    if (maybeSuccessorFrame.isDefined) {
      val txn = maybeSuccessorFrame.get
      val position = ensureReadVersion(txn, minPos)
      val version = _versions(position)
      // note: conversely, if a (no)change notification overtook discovery retrofitting, pending may change
      // from -1 to 0 here. No handling is required for this case, because firstFrame < position is an active
      // reevaluation (the one that's executing the discovery) and will afterwards progressToNextWrite, thereby
      // executing this then-ready reevaluation, but for now the version is guaranteed not stable yet.
      version.pending += arity
    }
    // cannot make this assertion here because dynamic events might make the firstFrame not a frame when dropping the only incoming changed dependency..
    //assertOptimizationsIntegrity(s"retrofitSinkFrames(writes=$successorWrittenVersions, maybeFrame=$maybeSuccessorFrame)")
    res
  }

  /**
    * collects transactions for retrofitting frames on the sink node
    * @param position the executing transaction's version's position
    * @return a list of transactions with written successor versions and maybe the transaction of the first successor
    *         frame if it exists, for which reframings have to be performed at the sink.
    */
  private def retrofitSourceOuts(position: Int): (List[T], Option[T]) = {
    // allocate array to the maximum number of written versions that might follow
    // (any version at index firstFrame or later can only be a frame, not written)
    val sizePrediction = math.max(firstFrame - position, 0)
    val successorWrittenVersions = new ArrayBuffer[T](sizePrediction)
    var maybeSuccessorFrame: Option[T] = None
    var stillCollecting = true
    for(pos <- position until size) {
      val version = _versions(pos)
      // as per above, this is implied false if pos >= firstFrame:
      if(stillCollecting) {
        if(version.isWritten){
          successorWrittenVersions += version.txn
        } else if (version.isFrame) {
          maybeSuccessorFrame = Some(version.txn)
          stillCollecting = false
        } else if (version.isOvertakeCompensation) {
          // something related to this case is probably not fully implemented correctly yet; such a test case wasn't priority yet.
          stillCollecting
        }
      }
    }
    if(successorWrittenVersions.size > sizePrediction) System.err.println(s"FullMV retrofitSourceOuts predicted size max($firstFrame - $position, 0) = $sizePrediction, but size eventually was ${successorWrittenVersions.size}")
    assertOptimizationsIntegrity(s"retrofitSourceOuts(from=$position) -> (writes=$successorWrittenVersions, maybeFrame=$maybeSuccessorFrame)")
    (successorWrittenVersions.toList, maybeSuccessorFrame)
  }

  def fullGC(): Int = synchronized {
    moveGCHintToLatestCompleted()
    gcAndLeaveHoles(_versions, _versions(latestGChint).value.isDefined, 0, -1, -1)
    lastGCcount
  }

  private def moveGCHintToLatestCompleted(): Unit = {
    @tailrec @inline def findLastCompleted(to: Int): Unit = {
      // gc = 0 = completed
      // to = 1 = !completed
      if (to > latestGChint) {
        val idx = latestGChint + (to - latestGChint + 1) / 2
        // 0 + (1 - 0 + 1) / 2 = 1
        if (_versions(idx).txn.phase == TurnPhase.Completed) {
          latestGChint = idx
          findLastCompleted(to)
        } else {
          findLastCompleted(idx - 1)
        }
      }
    }

    val latestPossibleGCHint = firstFrame - 1
    if (_versions(latestPossibleGCHint).txn.phase == TurnPhase.Completed) {
      // common case shortcut and corner case: all transactions that can be completed are completed (e.g., graph is in resting state)
      latestGChint = latestPossibleGCHint
    } else {
      findLastCompleted(firstFrame - 2)
    }
  }

  private def arrangeVersionArrayAndCreateVersions(insertOne: Int, one: T, insertTwo: Int, two: T): (Int, Int) = {
    arrangeVersionArray(2, insertOne, insertTwo)
    val first = insertOne - lastGCcount
    val second = insertTwo - lastGCcount + 1
    if(first == size) {
      val predVersion = _versions(size - 1)
      val lastWrittenPredecessorIfStable = computeSuccessorWrittenPredecessorIfStable(predVersion)
      val v1 = new Version(one, lastWrittenPredecessorIfStable, pending = 0, changed = 0, value = None)
      _versions(first) = v1
      indexedVersions.put(one, v1)
      val v2 = new Version(two, lastWrittenPredecessorIfStable, pending = 0, changed = 0, value = None)
      _versions(second) = v2
      indexedVersions.put(two, v2)
      if(lastWrittenPredecessorIfStable != null) firstFrame += 2
      size += 2
      assertOptimizationsIntegrity(s"arrangeVersionsAppend($insertOne -> $first, $one, $insertTwo -> $second, $two)")
      (first, second)
    } else {
      createVersionInHole(first, one)
      createVersionInHole(second, two)
      assertOptimizationsIntegrity(s"arrangeVersions($insertOne -> $first, $one, $insertTwo -> $second, $two)")
      (first, second)
    }
  }
  private def arrangeVersionArrayAndCreateVersion(insertPos: Int, txn: T): Int = {
    arrangeVersionArray(1, insertPos, -1)
    val pos = insertPos - lastGCcount
    createVersionInHole(pos, txn)
    assertOptimizationsIntegrity(s"arrangeVersions($insertPos -> $pos, $txn)")
    pos
  }

  private def arrangeVersionArray(create: Int, firstHole: Int, secondHole: Int): Unit = {
    assert(create != 0 || (firstHole < 0 && secondHole < 0), s"holes $firstHole and $secondHole do not match 0 insertions")
    assert(create != 1 || (firstHole >= 0 && secondHole < 0), s"holes $firstHole and $secondHole do not match 1 insertions")
    assert(create != 2 || (firstHole >= 0 && secondHole >= 0), s"holes $firstHole and $secondHole do not match 2 insertions")
    assert(secondHole < 0 || secondHole >= firstHole, s"second hole $secondHole must be behind or at first $firstHole")
    if(firstHole == size && size + create <= _versions.length) {
      // if only versions should be added at the end (i.e., existing versions don't need to be moved) and there's enough room, just don't do anything
      lastGCcount = 0
    } else {
      if (NodeVersionHistory.DEBUG_GC) println(s"[${Thread.currentThread().getName}] gc attempt to insert $create: ($firstHole, $secondHole) in $this")
      val hintVersionIsWritten = _versions(latestGChint).value.isDefined
      val straightDump = latestGChint - (if (hintVersionIsWritten) 0 else 1)
      if(straightDump == 0 && size + create <= _versions.length) {
        if (NodeVersionHistory.DEBUG_GC) println(s"[${Thread.currentThread().getName}] hintgc($latestGChint): -$straightDump would have no effect, but history rearrangement is possible")
        arrangeHolesWithoutGC(_versions, firstHole, secondHole)
      } else if (size - straightDump + create <= _versions.length) {
        if(NodeVersionHistory.DEBUG_GC) println(s"[${Thread.currentThread().getName}] hintgc($latestGChint): -$straightDump accepted")
        gcAndLeaveHoles(_versions, hintVersionIsWritten, create, firstHole, secondHole)
      } else {
        // straight dump with gc hint isn't enough: see what full GC brings
        if(NodeVersionHistory.DEBUG_GC) println(s"[${Thread.currentThread().getName}] hintgc($latestGChint): -$straightDump insufficient and not enough room for history rearrangement")
        moveGCHintToLatestCompleted()
        val fullGCVersionIsWritten = _versions(latestGChint).value.isDefined
        val fullDump = latestGChint - (if (fullGCVersionIsWritten) 0 else 1)
        if (size - fullDump + create <= _versions.length) {
          if(NodeVersionHistory.DEBUG_GC) println(s"[${Thread.currentThread().getName}] fullgc($latestGChint): -$fullDump accepted")
          gcAndLeaveHoles(_versions, fullGCVersionIsWritten, create, firstHole, secondHole)
        } else {
          // full GC also isn't enough either: grow the array.
          val grown = new Array[Version](_versions.length + (_versions.length >> 1))
          if(fullDump == 0) {
            if(NodeVersionHistory.DEBUG_GC) println(s"[${Thread.currentThread().getName}] fullgc($latestGChint): -$fullDump would have no effect, rearraging after growing max size ${_versions.length} -> ${grown.length}")
            if(firstHole > 0) System.arraycopy(_versions, 0, grown, 0, firstHole)
            arrangeHolesWithoutGC(grown, firstHole, secondHole)
          } else {
            if(NodeVersionHistory.DEBUG_GC) println(s"[${Thread.currentThread().getName}] fullgc($latestGChint): -$fullDump insufficient, also growing max size ${_versions.length} -> ${grown.length}")
            gcAndLeaveHoles(grown, fullGCVersionIsWritten, create, firstHole, secondHole)
          }
          _versions = grown
        }
      }
      if(NodeVersionHistory.DEBUG_GC) println(s"[${Thread.currentThread().getName}] after gc of $lastGCcount, holes at (${if(firstHole == -1) -1 else firstHole - lastGCcount}, ${if(secondHole == -1) -1 else secondHole - lastGCcount + 1}): $this")
    }
  }

  private def arrangeHolesWithoutGC(writeTo: Array[Version], firstHole: Int, secondHole: Int): Unit = {
    if (firstHole >= 0 && firstHole < size) {
      if (secondHole < 0 || secondHole == size) {
        System.arraycopy(_versions, firstHole, writeTo, firstHole + 1, size - firstHole)
      } else {
        System.arraycopy(_versions, secondHole, writeTo, secondHole + 2, size - secondHole)
        if (secondHole != firstHole) System.arraycopy(_versions, firstHole, writeTo, firstHole + 1, secondHole - firstHole)
      }
    }
    lastGCcount = 0
  }

  private def gcAndLeaveHoles(writeTo: Array[Version], hintVersionIsWritten: Boolean, create: Int, firstHole: Int, secondHole: Int): Unit = {
    // if a straight dump using the gc hint makes enough room, just do that
    if (hintVersionIsWritten) {
      if(NodeVersionHistory.DEBUG_GC) println(s"[${Thread.currentThread().getName}] hint is written: dumping $latestGChint to offset 0")
      // if hint is written, just dump everything before
      latestReevOut -= latestGChint
      dumpToOffsetAndLeaveHoles(writeTo, latestGChint, null, firstHole, secondHole)
      lastGCcount = latestGChint
    } else {
      // otherwise find the latest write before the hint, move it to index 0, and only dump everything else
      lastGCcount = latestGChint - 1
      latestReevOut = if(latestReevOut <= latestGChint) 0 else latestReevOut - lastGCcount
      dumpToOffsetAndLeaveHoles(writeTo, latestGChint, _versions(latestGChint).lastWrittenPredecessorIfStable, firstHole, secondHole)
    }
    writeTo(0).lastWrittenPredecessorIfStable = null
    val sizeBefore = size
    latestGChint -= lastGCcount
    firstFrame -= lastGCcount
    size -= lastGCcount
    if ((_versions eq writeTo) && size + create < sizeBefore) java.util.Arrays.fill(_versions.asInstanceOf[Array[AnyRef]], size + create, sizeBefore, null)
  }

  private def dumpToOffsetAndLeaveHoles(writeTo: Array[Version], retainFrom: Int, retainOnZero: Version, firstHole: Int, secondHole: Int): Unit = {
    assert(firstHole >= 0 || secondHole < 0, "must not give only a second hole")
    assert(secondHole < 0 || secondHole >= firstHole, "second hole must be behind or at first")

//    println(s"dumpToOffsetAndLeaveHoles($writeTo, $retainFrom, $retainOnZero, $firstHole, $secondHole) on $this:")
    for(i <- 0 until retainFrom) {
      val dump = _versions(i)
      if(dump ne retainOnZero) {
        val removed = indexedVersions.remove(dump.txn)
        assert(removed eq dump, s"removal of $i from index $indexedVersions returned $removed instead of $dump in $this")
//        println(s"removed $dump from index")
      }
    }

    val retainTo = if(retainOnZero == null) {
      0
    } else {
      writeTo(0) = retainOnZero
      1
    }

    // just dump everything before the hint
    if (firstHole < 0 || firstHole == size) {
      // no hole or holes at the end only: the entire array stays in one segment
      System.arraycopy(_versions, retainFrom, writeTo, retainTo, size - retainFrom)
    } else {
      // copy first segment
      System.arraycopy(_versions, retainFrom, writeTo, retainTo, firstHole - retainFrom)
      val gcOffset = retainTo - retainFrom
      val newFirstHole = gcOffset + firstHole
      if (secondHole < 0 || secondHole == size) {
        // no second hole or second hole at the end only: there are only two segments
        if((_versions ne writeTo) || gcOffset != 1) System.arraycopy(_versions, firstHole, writeTo, newFirstHole + 1, size - firstHole)
      } else {
        if (secondHole != firstHole && ((_versions ne writeTo) || gcOffset != 1)) System.arraycopy(_versions, firstHole, writeTo, newFirstHole + 1, secondHole - firstHole)
        if((_versions ne writeTo) || gcOffset != 2) System.arraycopy(_versions, secondHole, writeTo, gcOffset + secondHole + 2, size - secondHole)
      }
    }
//    println(s"dumpToOffsetAndLeaveHoles($writeTo, $retainFrom, $retainTo, $firstHole, $secondHole) result: $this")
  }
}

object NodeVersionHistory {
  val DEBUG_GC = false

  sealed trait TryOrderResult
  case object FailedNonfinal extends TryOrderResult
  sealed trait TryRecordResult extends TryOrderResult
  case object Succeeded extends TryRecordResult
  case object FailedFinalAndRecorded extends TryRecordResult
}
