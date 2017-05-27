package rescala.fullmv

import java.util.concurrent.ForkJoinPool
import java.util.concurrent.ForkJoinPool.ManagedBlocker

import rescala.engine.ValuePersistency

import scala.annotation.elidable.ASSERTION
import scala.annotation.{elidable, tailrec}
import scala.collection.mutable.ArrayBuffer

sealed trait FramingBranchResult[+T, +R]
object FramingBranchResult {
  case object FramingBranchEnd extends FramingBranchResult[Nothing, Nothing]
  case class FramingBranchOut[R](out: Set[R]) extends FramingBranchResult[Nothing, R]
  case class FramingBranchOutSuperseding[T, R](out: Set[R], supersede: T) extends FramingBranchResult[T, R]
}

sealed trait NotificationResultAction[+T, +R]
object NotificationResultAction {
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
  * @param sgt the transaction ordering authority
  * @param init the initial creating transaction
  * @param valuePersistency the value persistency descriptor
  * @tparam V the type of stored values
  * @tparam T the type of transactions
  * @tparam R the type of dependency nodes (reactives)
  */
class NodeVersionHistory[V, T <: TurnPhase, R](val sgt: SerializationGraphTracking[T], init: T, val valuePersistency: ValuePersistency[V]) {
  trait BlockOnHistoryManagedBlocker extends ManagedBlocker {
    override def block(): Boolean = NodeVersionHistory.this.synchronized {
      isReleasable || { NodeVersionHistory.this.wait(); isReleasable }
    }
  }
  private class Version(val txn: T, var stable: Boolean, var out: Set[R], var pending: Int, var changed: Int, var value: Option[V]) extends BlockOnHistoryManagedBlocker {
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
        _blockForStable = new BlockOnHistoryManagedBlocker {
          override def isReleasable: Boolean = NodeVersionHistory.this.synchronized {
            stable
          }
        }
      }
      _blockForStable
    }


    override def toString: String = {
      if(isWritten){
        "Written(" + txn + ", out=" + out + ", v=" + value.get + ")"
      } else if (isReadOrDynamic) {
        (if(stable) "Stable" else "Unstable") + "Marker(" + txn + ", out=" + out + ")"
      } else if (isOvertakeCompensation) {
        "OvertakeCompensation(" + txn + ", " + (if (stable) "stable" else "unstable") + ", out=" + out + ", pending=" + pending + ", changed=" + changed + ")"
      } else if(isFrame) {
        if(stable) {
          if(isReadyForReevaluation) {
            "Active(" + txn + ", out=" + out + ")"
          } else {
            "FirstFrame(" + txn + ", out=" + out + ", pending=" + pending + ", changed=" + changed + ")"
          }
        } else {
          if(isReadyForReevaluation) {
            "Queued(" + txn + ", out=" + out + ")"
          } else {
            "Frame(" + txn + ", out=" + out + ", pending=" + pending + ", changed=" + changed + ")"
          }
        }
      } else {
        "UnknownVersionCase!(" + txn + ", " + (if(stable) "stable" else "unstable") + ", out=" + out + ", pending=" + pending + ", changed=" + changed + ", value = " + value + ")"
      }
    }
  }

  @elidable(ASSERTION) @inline
  def assertOptimizationsIntegrity(debugOutputDescription: => String): Unit = {
    def debugStatement(whatsWrong: String): String = s"$debugOutputDescription left $whatsWrong (latestWritten $latestWritten, ${if(firstFrame > _versions.size) "no frames" else "firstFrame "+firstFrame}): \n  " + _versions.zipWithIndex.map{case (version, index) => s"$index: $version"}.mkString("\n  ")

    assert(firstFrame >= _versions.size || _versions(firstFrame).isFrame, debugStatement("firstFrame not frame"))
    assert(!_versions.take(firstFrame).exists(_.isFrame), debugStatement("firstFrame not first"))

    assert(_versions(latestWritten).isWritten, debugStatement("latestWritten not written"))
    assert(!_versions.drop(latestWritten + 1).exists(_.isWritten), debugStatement("latestWritten not latest"))

    assert(!_versions.zipWithIndex.exists{case (version, index) => version.stable != (index <= firstFrame)}, debugStatement("broken version stability"))
  }

  // =================== STORAGE ====================

  private val _versions = new ArrayBuffer[Version](6)
  _versions += new Version(init, stable = true, out = Set(), pending = 0, changed = 0, Some(valuePersistency.initialValue))
  var latestValue: V = valuePersistency.initialValue

  var incomings: Set[R] = Set.empty

  /**
    * creates a version and inserts it at the given position; default parameters create a "read" version
    * @param position the position
    * @param txn the transaction this version is associated with
    * @param pending the number of pending notifications, none by default
    * @param changed the number of already received change notifications, none by default
    * @return the created version
    */
  private def createVersion(position: Int, txn: T, pending: Int = 0, changed: Int = 0): Version = {
    assert(position > 0, "cannot create a version at negative position")
    val version = new Version(txn, stable = position <= firstFrame, _versions(position - 1).out, pending, changed, None)
    _versions.insert(position, version)
    if(position <= firstFrame) firstFrame += 1
    if(position <= latestWritten) latestWritten += 1
    version
  }


  // =================== NAVIGATION ====================
  var firstFrame: Int = _versions.size
  var latestWritten: Int = 0

  /**
    * performs binary search for the given transaction in _versions. If no version associated with the given
    * transaction is found, it establishes an order in sgt against all other versions' transactions, placing
    * the given transaction as late as possible. For this it first finds the latest possible insertion point. Assume,
    * for a given txn, this is between versions y and z. As the latest point is used, it is txn < z.txn. To fixate this
    * order, the search thus attempts to establish y.txn < txn in sgt. This may fail, however, if concurrent
    * threads intervened and established txn < y.txn. For this case, the search keeps track of the latest x for which
    * x.txn < txn is already established. It then restarts the search in the reduced fallback range between x and y.
    * @param lookFor the transaction to look for
    * @param recoverFrom current fallback from index in case of order establishment fallback
    * @param from current from index of the search range
    * @param to current to index (exclusive) of the search range
    * @return the index of the version associated with the given transaction, or if no such version exists the negative
    *         index it would have to be inserted. Note that we assume no insertion to ever occur at index 0 -- the
    *         the first version of any node is either from the transaction that created it or from some completed
    *         transaction. In the former case, no preceding transaction should be aware of the node's existence. In the
    *         latter case, there are no preceding transactions that still execute operations. Thus insertion at index 0
    *         should be impossible. A return value of 0 thus means "found at 0", rather than "should insert at 0"
    */
  @tailrec
  private def findOrPidgeonHole(lookFor: T, recoverFrom: Int, from: Int, to: Int): Int = {
    if (to == from) {
      assert(from > 0, s"Found an insertion point of 0 for $lookFor; insertion points must always be after the base state version.")
      if(sgt.ensureOrder(_versions(from - 1).txn, lookFor) == FirstFirst) {
        -from
      } else {
        assert(recoverFrom < from, s"Illegal position hint: $lookFor must be before $recoverFrom")
        findOrPidgeonHole(lookFor, recoverFrom, recoverFrom, from)
      }
    } else {
      val idx = from+(to-from-1)/2
      val candidate = _versions(idx).txn
      if(candidate == lookFor) {
        idx
      } else sgt.getOrder(candidate, lookFor) match {
        case FirstFirst =>
          findOrPidgeonHole(lookFor, idx + 1, idx + 1, to)
        case SecondFirst =>
          findOrPidgeonHole(lookFor, recoverFrom, from, idx)
        case Unordered =>
          findOrPidgeonHole(lookFor, recoverFrom, idx + 1, to)
      }
    }
  }

  /**
    * determine the position or insertion point for a framing transaction
    *
    * @param txn the transaction
    * @return the position (positive values) or insertion point (negative values)
    */
  private def findFrame(txn: T): Int = {
    findOrPidgeonHole(txn, latestWritten, latestWritten, _versions.size)
  }

  /**
    * determine the position or insertion point for a transaction for which
    * this node is known to have become final
    *
    * @param txn the transaction
    * @return the position (positive values) or insertion point (negative values)
    */
  private def findFinal(txn: T) = {
    findOrPidgeonHole(txn, 1, 1, firstFrame)
  }


  // =================== FRAMING ====================

  /**
    * entry point for superseding framing
    * @param txn the transaction visiting the node for framing
    * @param supersede the transaction whose frame was superseded by the visiting transaction at the previous node
    */
  def incrementSupersedeFrame(txn: T, supersede: T): FramingBranchResult[T, R] = synchronized {
    val result = incrementFrame0(txn)
    val supersedePosition = findFrame(supersede)
    if (supersedePosition >= 0) {
      _versions(supersedePosition).pending -= 1
    } else {
      createVersion(-supersedePosition, supersede, pending = -1)
    }
    assertOptimizationsIntegrity(s"incrementSupersedeFrame($txn, $supersede) -> $result")
    result
  }

  /**
    * entry point for regular framing
    * @param txn the transaction visiting the node for framing
    */
  def incrementFrame(txn: T): FramingBranchResult[T, R] = synchronized {
    val result = incrementFrame0(txn)
    assertOptimizationsIntegrity(s"incrementFrame($txn) -> $result")
    result
  }

  /**
    * creates a frame if none exists, or increments an existing frame's [[Version.pending]] counter.
    * @param txn the transaction visiting the node for framing
    * @return a descriptor of how this framing has to propagate
    */
  private def incrementFrame0(txn: T): FramingBranchResult[T, R] = {
    maybeGC()
    val position = findFrame(txn)
    if(position >= 0) {
      val version = _versions(position)
      version.pending += 1
      if(version.pending == 1) {
        // this case (a version is framed for the "first" time, i.e., pending == 1, during framing, but already existed
        // beforehand) is relevant if drop retrofitting downgraded a frame into a marker and the marker and subsequent
        // versions were subsequently stabilized. Since this marker is now upgraded back into a frame, subsequent
        // versions need to be unstabilized again. This is safe to do because this method is only called by framing
        // transactions, meaning all subsequent versions must also be frames, deleted frame markers, or to be deleted
        // frame drop compensations. This case cannot occur for follow framings attached to change notifications,
        // because these imply a preceding reevaluation, meaning neither the incremented frame nor any subsequent
        // frame can be stable yet.
        frameCreated(position, version)
      } else {
        FramingBranchResult.FramingBranchEnd
      }
    } else {
      val frame = createVersion(-position, txn, pending = 1)
      frameCreated(-position, frame)
    }
  }

  private def frameCreated(position: Int, frame: Version): FramingBranchResult[T, R] = {
    if(position < firstFrame) {
      for(pos <- (position + 1) until firstFrame) {
        assert(_versions(pos).stable, s"${_versions(position).txn} cannot destabilize ${_versions(pos)}")
        _versions(pos).stable = false
      }
      val result = if(firstFrame < _versions.size) {
        assert(_versions(firstFrame).stable, s"${_versions(position).txn} cannot destabilize firstframe ${_versions(firstFrame)}")
        _versions(firstFrame).stable = false
        FramingBranchResult.FramingBranchOutSuperseding(frame.out, _versions(firstFrame).txn)
      } else {
        FramingBranchResult.FramingBranchOut(frame.out)
      }
      firstFrame = position
      result
    } else {
      FramingBranchResult.FramingBranchEnd
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
  def notify(txn: T, changed: Boolean): NotificationResultAction[T, R] = synchronized {
    val result = notify0(findFrame(txn), txn, changed)
    assertOptimizationsIntegrity(s"notify($txn, $changed) -> $result")
    result
  }

  /**
    * entry point for change/nochange notification reception with follow-up framing
    * @param txn the transaction sending the notification
    * @param changed whether or not the dependency changed
    * @param followFrame a transaction for which to create a subsequent frame, furthering its partial framing.
    */
  def notifyFollowFrame(txn: T, changed: Boolean, followFrame: T): NotificationResultAction[T, R] = synchronized {
    maybeGC()
    val position = findFrame(txn)

    // do follow framing
    val followFrameMinPosition = if (position > 0) position + 1 else -position
    // because we know that txn << followTxn, followTxn cannot be involved with firstFrame stuff.
    // thus followTxn also does not need this framing propagated by itself.
    val followPosition = findOrPidgeonHole(followFrame, followFrameMinPosition, followFrameMinPosition, _versions.size)
    if (followPosition >= 0) {
      _versions(followPosition).pending += 1
    } else {
      createVersion(-followPosition, followFrame, pending = 1)
    }

    val result = notify0(findFrame(txn), txn, changed)
    assertOptimizationsIntegrity(s"notifyFollowFrame($txn, $changed, $followFrame) -> $result")
    result
  }

  def notify0(position: Int, txn: T, changed: Boolean): NotificationResultAction[T, R] = {
    if(position < 0) {
      assert(-position != firstFrame, "(no)change notification, which overtook (a) discovery retrofitting or " +
        "(b) predecessor (no)change notification with followFraming for this transaction, wants to create FirstFrame, " +
        "which should be impossible because firstFrame should be (a) the predecessor reevaluation performing said retrofitting or " +
        "(b) the predecessor reevaluation pending reception of said (no)change notification.")
      // note: this case occurs if a (no)change notification overtook discovery retrofitting, and sets pending to -1!
      // In this case, we simply return the results as if discovery retrofitting had already happened: As we know
      // that it is still in progress, there must be a preceding active reevaluation (hence above assertion), so the
      // retrofitting would simply create a queued frame, which this notification would either:
      if(changed) {
        // convert into a queued reevaluation
        createVersion(-position, txn, pending = -1, changed = 1)
        NotificationResultAction.GlitchFreeReadyButQueued
      } else {
        // or resolve into an unchanged marker
        createVersion(-position, txn, pending = -1)
        NotificationResultAction.ResolvedNonFirstFrameToUnchanged
      }
    } else {
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
            progressToNextWriteForNotification(s"$this notify ResolvedFirstFrame($position)ToUnchanged", version.out)
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
  }

  def reevIn(turn: T): V = {
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
  def reevOut(turn: T, maybeValue: Option[V]): NotificationResultAction.NotificationOutAndSuccessorOperation[T, R] = synchronized {
    val position = firstFrame
    val version = _versions(position)
    assert(version.txn == turn, s"$turn called reevDone, but first frame is $version (different transaction)")
    assert(version.value.isEmpty, s"cannot write twice: $version")
    assert(!version.isOvertakeCompensation && (version.isReadyForReevaluation || (version.isReadOrDynamic && maybeValue.isEmpty)), s"cannot write $version")

    if(maybeValue.isDefined) {
      latestWritten = position
      this.latestValue = maybeValue.get
      version.value = maybeValue
    }
    version.changed = 0

    val result = progressToNextWriteForNotification(s"$this ReevOut($position)${if(maybeValue.isDefined) "Changed" else "Unchanged"}", version.out)
    assertOptimizationsIntegrity(s"reevOut($turn, ${maybeValue.isDefined}) -> $result")
    result
  }

  /**
    * progresses [[firstFrame]] forward until a [[Version.isFrame]] is encountered and assemble all necessary
    * information to send out change/nochange notifications for the given transaction. Also capture synchronized,
    * whether or not the possibly encountered write [[Version.isReadyForReevaluation]].
    * @return the notification and next reevaluation descriptor.
    */
  private def progressToNextWriteForNotification(debugOutputString: => String, out: Set[R]): NotificationResultAction.NotificationOutAndSuccessorOperation[T, R] = {
    @tailrec
    def progressToNextWriteForNotification0(): NotificationResultAction.NotificationOutAndSuccessorOperation[T, R] = {
      firstFrame += 1
      if (firstFrame < _versions.size) {
        val version = _versions(firstFrame)

        assert(!version.stable, s"$debugOutputString cannot stabilize already-stable $version at position $firstFrame")
        assert(!version.isWritten, s"$debugOutputString found unstable but written $version at position $firstFrame")
        version.stable = true

        if (version.isFrame) {
          if (version.isReadyForReevaluation) {
            NotificationResultAction.NotificationOutAndSuccessorOperation.NextReevaluation(out, version.txn)
          } else {
            NotificationResultAction.NotificationOutAndSuccessorOperation.FollowFraming(out, version.txn)
          }
        } else {
          progressToNextWriteForNotification0()
        }
      } else {
        NotificationResultAction.NotificationOutAndSuccessorOperation.NoSuccessor(out)
      }
    }
    val res = progressToNextWriteForNotification0()
    notifyAll()
    res
  }

  // =================== READ OPERATIONS ====================

  /**
    * ensures at least a read version is stored to track executed reads or dynamic operations.
    * @param txn the executing transaction
    * @return the version's position.
    */
  private def ensureReadVersion(txn: T, knownMinPos: Int = 0): Int = {
    val position = findOrPidgeonHole(txn, knownMinPos, knownMinPos, _versions.size)
    if(position < 0) {
      createVersion(-position, txn)
      -position
    } else {
      position
    }
  }

  /**
    * entry point for before(this); may suspend.
    *
    * @param txn the executing transaction
    * @return the corresponding [[Version.value]] from before this transaction, i.e., ignoring the transaction's
    *         own writes.
    */
  def dynamicBefore(txn: T): V = synchronized {
    assert(!valuePersistency.isTransient, s"$txn invoked dynamicBefore on transient node")
    maybeGC()
    synchronized {
      val position = ensureReadVersion(txn)
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
          val stablePosition = if(version.isFrame) firstFrame else findFinal(txn)
          assert(stablePosition >= 0, "somehow, the version allocated above disappeared..")
          before(txn, stablePosition)
        }
    }
  }

  def staticBefore(txn: T): V = synchronized {
    before(txn, math.abs(findFinal(txn)))
  }

  private def before(txn: T, position: Int): V = synchronized {
    assert(!valuePersistency.isTransient, "before read on transient node")
    assert(position > 0, s"$txn cannot read before first version")
    val lastWrite = lastWriteUpTo(position - 1)
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
  def dynamicAfter(txn: T): V = {
    synchronized {
      maybeGC()
      val position = ensureReadVersion(txn)
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
            beforeOrInit(txn, findFinal(txn))
          }
        }
    }
  }

  def staticAfter(txn: T): V = synchronized {
    val position = findFinal(txn)
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
  def discover(txn: T, add: R): (ArrayBuffer[T], Option[T]) = synchronized {
    val position = ensureReadVersion(txn)
    assertOptimizationsIntegrity(s"ensureReadVersion($txn)")
    assert(!_versions(position).out.contains(add), "must not discover an already existing edge!")
    retrofitSourceOuts(position, add, +1)
  }

  /**
    * entry point for drop(this, ticket.issuer); may suspend temporarily.
    * @param txn the executing reevaluation's transaction
    * @param remove the removed edge's sink node
    */
  def drop(txn: T, remove: R): (ArrayBuffer[T], Option[T]) = synchronized {
    val position = ensureReadVersion(txn)
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
  def retrofitSinkFrames(successorWrittenVersions: ArrayBuffer[T], maybeSuccessorFrame: Option[T], arity: Int): Unit = synchronized {
    require(math.abs(arity) == 1)
    var minPos = firstFrame
    for(txn <- successorWrittenVersions) {
      val position = ensureReadVersion(txn, minPos)
      val version = _versions(position)
      // note: if drop retrofitting overtook a change notification, changed may update from 0 to -1 here!
      version.changed += arity
      minPos = position + 1
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
  private def retrofitSourceOuts(position: Int, delta: R, arity: Int): (ArrayBuffer[T], Option[T]) = {
    require(math.abs(arity) == 1)
    // allocate array to the maximum number of written versions that might follow
    // (any version at index firstFrame or later can only be a frame, not written)
    val successorWrittenVersions = new ArrayBuffer[T](firstFrame - position - 1)
    for(pos <- position until _versions.size) {
      val version = _versions(pos)
      if(arity < 0) version.out -= delta else version.out += delta
      // as per above, this is implied false if pos >= firstFrame:
      if(version.isWritten) successorWrittenVersions += version.txn
    }
    val maybeSuccessorFrame = if (firstFrame < _versions.size) Some(_versions(firstFrame).txn) else None
    (successorWrittenVersions, maybeSuccessorFrame)
  }

  private def maybeGC(): Int = {
    // placeholder in case we want to have some mechanic that reduces the frequency of GC runs
    if(true) {
      gcObsoleteVersions()
    } else {
      0
    }
  }

  def gcObsoleteVersions(): Int = synchronized {
    if(_versions.size > 1) {
      @tailrec
      def findLastCompleted(from: Int, to: Int): Int = {
        if (to < from) {
          if (_versions(from).txn.phase == TurnPhase.Completed) {
            from
          } else {
            0
          }
        } else {
          val idx = from + (to - from - 1) / 2
          val candidate = _versions(idx).txn
          if (candidate.phase == TurnPhase.Completed) {
            if (_versions(idx + 1).txn.phase == TurnPhase.Completed) {
              findLastCompleted(idx + 1, to)
            } else {
              idx
            }
          } else {
            if (_versions(idx - 1).txn.phase == TurnPhase.Completed) {
              idx - 1
            } else {
              findLastCompleted(from, idx - 1)
            }
          }
        }
      }

      val lastCompleted = findLastCompleted(1, math.min(_versions.size - 2, firstFrame))
      assert(_versions(lastCompleted).txn.phase == TurnPhase.Completed)
      if (lastCompleted > 0) {
        if (_versions(lastCompleted).value.isDefined) {
          firstFrame -= lastCompleted
          latestWritten -= lastCompleted
          _versions.remove(0, lastCompleted)
          lastCompleted
        } else if(lastCompleted > 1) {
          val dumpCount = lastCompleted - 1
          firstFrame -= dumpCount
          _versions(0) = if(_versions(latestWritten).txn.phase == TurnPhase.Completed) {
            val result = _versions(latestWritten)
            latestWritten = 0
            result
          } else {
            latestWritten -= dumpCount
            lastWriteUpTo(lastCompleted)
          }
          _versions.remove(1, dumpCount)
          dumpCount
        } else {
          0
        }
      } else {
        0
      }
    } else {
      0
    }
  }
}
