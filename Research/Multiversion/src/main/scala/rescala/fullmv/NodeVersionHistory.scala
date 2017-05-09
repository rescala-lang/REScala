package rescala.fullmv


import rescala.engine.ValuePersistency
import rescala.graph.Reactive

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class Version[D, R](val txn: FullMVTurn, var stable: Boolean, var out: Set[Reactive[FullMVStruct]], var pending: Int, var changed: Int, var value: Option[D]) {
  def isWritten: Boolean = changed == 0 && value.isDefined
  def isFrame: Boolean = pending > 0 || (changed > 0 && !isWritten)
  def isWrite: Boolean = pending > 0 || changed > 0 || isWritten
  def isReadOrDynamic: Boolean = !isWrite
  def isReadyForReevaluation: Boolean = pending == 0 && changed > 0
  def read(): D = {
    assert(isWritten, "reading un-written "+this)
    value.get
  }

  override def toString: String = "Version("+txn+", stable="+stable+", out="+out+", pending="+pending+", changed="+changed+", "+value+")"
}

sealed trait FramingBranchResult
object FramingBranchResult {
  case object FramingBranchEnd extends FramingBranchResult
  case class FramingBranchOut(out: Set[Reactive[FullMVStruct]]) extends FramingBranchResult
  case class FramingBranchOutSuperseding(out: Set[Reactive[FullMVStruct]], supersede: FullMVTurn) extends FramingBranchResult
}

sealed trait NotificationResultAction
object NotificationResultAction {
  case object NotGlitchFreeReady extends NotificationResultAction
  case object ResolvedQueuedToUnchanged extends NotificationResultAction
  case object GlitchFreeReadyButQueued extends NotificationResultAction
  case object GlitchFreeReady extends NotificationResultAction
  sealed trait NotificationOutAndSuccessorOperation extends NotificationResultAction {
    val out: Set[Reactive[FullMVStruct]]
  }
  object NotificationOutAndSuccessorOperation {
    case class NoSuccessor(out: Set[Reactive[FullMVStruct]]) extends NotificationOutAndSuccessorOperation
    case class FollowFraming(out: Set[Reactive[FullMVStruct]], succTxn: FullMVTurn) extends NotificationOutAndSuccessorOperation
    case class NextReevaluation(out: Set[Reactive[FullMVStruct]], succTxn: FullMVTurn) extends NotificationOutAndSuccessorOperation
  }
}

class NodeVersionHistory[V](val sgt: SerializationGraphTracking, init: FullMVTurn, val valuePersistency: ValuePersistency[V]) {
  type R = Reactive[FullMVStruct]
  // =================== STORAGE ====================

  var _versions = new ArrayBuffer[Version[V, R]](6)
  _versions += new Version[V, R](init, stable = true, out = Set(), pending = 0, changed = 0, Some(valuePersistency.initialValue))
  var latestValue: V = valuePersistency.initialValue

  var incomings: Set[Reactive[FullMVStruct]] = Set.empty

  /**
    * creates a version and inserts it at the given position; default parameters create a "read" version
    * @param position the position
    * @param txn the FullMVTurn
    *           this version is associated with
    * @param pending the number of pending notifications, none by default
    * @param changed the number of already received change notifications, none by default
    * @return the created version
    */
  private def createVersion(position: Int, txn: FullMVTurn, pending: Int = 0, changed: Int = 0): Version[V, R] = {
    assert(position > 0, "cannot create a version at negative position")
    val version = new Version[V, R](txn, stable = position <= firstFrame, _versions(position - 1).out, pending, changed, None)
    _versions.insert(position, version)
    if(position <= firstFrame) firstFrame += 1
    version
  }


  // =================== NAVIGATION ====================
  var firstFrame: Int = _versions.size

  /**
    * performs binary search for the given FullMVTurn
    *in _versions. If no version associated with the given
    * FullMVTurn
    *is found, it establishes an order in sgt against all other versions' FullMVTurn
    *s, placing
    * the given FullMVTurn
    *as late as possible. For this it first finds the latest possible insertion point. Assume,
    * for a given txn, this is between versions y and z. As the latest point is used, it is txn < z.txn. To fixate this
    * order, the search thus attempts to establish y.txn < txn in sgt. This may fail, however, if concurrent
    * threads intervened and established txn < y.txn. For this case, the search keeps track of the latest x for which
    * x.txn < txn is already established. It then restarts the search in the reduced fallback range between x and y.
    * @param lookFor the FullMVTurn to look for
    * @param recoverFrom current fallback from index in case of order establishment fallback
    * @param from current from index of the search range
    * @param to current to index (exclusive) of the search range
    * @return the index of the version associated with the given FullMVTurn
    *        , or if no such version exists the negative
    *         index it would have to be inserted. Note that we assume no insertion to ever occur at index 0 -- the
    *         the first version of any node is either from the FullMVTurn
    *        that created it or from some completed
    *         FullMVTurn
    *        . In the former case, no preceding FullMVTurn
    *        should be aware of the node's existence. In the
    *         latter case, there are no preceding FullMVTurn
    *        s that still execute operations. Thus insertion at index 0
    *         should be impossible. A return value of 0 thus means "found at 0", rather than "should insert at 0"
    */
  @tailrec
  private def findOrPidgeonHole(lookFor: FullMVTurn, recoverFrom: Int, from: Int, to: Int): Int = {
    if (to == from) {
      assert(from > 0, "Found an insertion point of 0; insertion points must always be after the base state version.")
      if(sgt.ensureOrder(_versions(math.abs(from) - 1).txn, lookFor) == FirstFirst) {
        -from
      } else {
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
    * determine the position or insertion point for a framing FullMVTurn
    *
    * @param txn the FullMVTurn
    * @return the position (positive values) or insertion point (negative values)
    */
  private def findFrame(txn: FullMVTurn): Int = {
    findOrPidgeonHole(txn, firstFrame, firstFrame, _versions.size)
  }

  /**
    * traverse history back in time from the given position until the first write (i.e. [[Version.isWrite]])
    * @param txn the FullMVTurn for which the previous write is being searched; used for error reporting only.
    * @param from the FullMVTurn's position, search starts at this position's predecessor version and moves backwards
    * @return the first encountered version with [[Version.isWrite]]
    */
  private def prevWrite(txn: FullMVTurn, from: Int): Version[V, R] = {
    var position = from - 1
    while(position >= 0 && !_versions(position).isWrite) position -= 1
    if(position < 0) throw new IllegalArgumentException(txn + " does not have a preceding write in versions " + _versions.take(from))
    _versions(position)
  }

  // =================== FRAMING ====================

  /**
    * entry point for superseding framing
    * @param txn the FullMVTurn visiting the node for framing
    * @param supersede the FullMVTurn whose frame was superseded by the visiting FullMVTurn
    *                 at the previous node
    */
  def incrementSupersedeFrame(txn: FullMVTurn, supersede: FullMVTurn): FramingBranchResult = {
//    assert(txn.phase == Preparing)
//    assert(supersede.phase == Preparing)
    synchronized {
      val position = findFrame(supersede)
      if (position >= 0) {
        _versions(position).pending -= 1
      } else {
        createVersion(-position, supersede, pending = -1)
      }
      incrementFrame0(txn)
    }
  }

  /**
    * entry point for regular framing
    * @param txn the FullMVTurn visiting the node for framing
    */
  def incrementFrame(txn: FullMVTurn): FramingBranchResult = {
//    assert(txn.phase == Preparing)
    synchronized { incrementFrame0(txn) }
  }

  /**
    * creates a frame if none exists, or increments an existing frame's [[Version.pending]] counter.
    * @param txn the FullMVTurn visiting the node for framing
    * @return a descriptor of how this framing has to propagate
    */
  private def incrementFrame0(txn: FullMVTurn): FramingBranchResult = {
    val position = findFrame(txn)
    if(position >= 0) {
      _versions(position).pending += 1
      FramingBranchResult.FramingBranchEnd
    } else {
      val frame = createVersion(-position, txn, pending = 1)
      val previousFirstFrame = firstFrame
      if(-position < firstFrame) {
        firstFrame = -position
      }
      if(frame.out.isEmpty) {
        FramingBranchResult.FramingBranchEnd
      } else if(-position < previousFirstFrame) {
        if(previousFirstFrame < _versions.size) {
          val supersede = _versions(previousFirstFrame).txn
          FramingBranchResult.FramingBranchOutSuperseding(frame.out, supersede)
        } else {
          FramingBranchResult.FramingBranchOut(frame.out)
        }
      } else {
        FramingBranchResult.FramingBranchEnd
      }
    }
  }

  /*
   * =================== NOTIFICATIONS/ / REEVALUATION ====================
   */

  /**
    * entry point for change/nochange notification reception
    * @param txn the FullMVTurn sending the notification
    * @param changed whether or not the dependency changed
    * @param maybeFollowFrame possibly a FullMVTurn
    *                        for which to create a subsequent frame, furthering its partial framing.
    */
  def notify(txn: FullMVTurn, changed: Boolean, maybeFollowFrame: Option[FullMVTurn]): NotificationResultAction = synchronized {
    val position = findFrame(txn)

    // do follow framing
    if(maybeFollowFrame.isDefined) {
      val followTxn = maybeFollowFrame.get
      // because we know that txn << followTxn, followTxn cannot be involved with firstFrame stuff.
      // thus followTxn also does not need this framing propagated by itself.
      val followPosition = findOrPidgeonHole(followTxn, position + 1, position + 1, _versions.size)
      if(followPosition >= 0) {
        _versions(followPosition).pending += 1
      } else {
        createVersion(-followPosition, followTxn, pending = 1)
      }
    }

    // apply notification changes
    val version = _versions(position)
    version.pending -= 1
    if(changed) version.changed += 1

    // check if the notification triggers subsequent actions
    if(version.pending == 0) {
      if(position == firstFrame) {
        if(version.changed > 0) {
          NotificationResultAction.GlitchFreeReady
        } else {
          // ResolvedFirstFrameToUnchanged
          progressToNextWriteForNotification(version.out)
        }
      } else {
        if(version.changed > 0) {
          NotificationResultAction.GlitchFreeReadyButQueued
        } else {
          NotificationResultAction.ResolvedQueuedToUnchanged
        }
      }
    } else {
      NotificationResultAction.NotGlitchFreeReady
    }
  }

  def reevIn(turn: FullMVTurn): (V, Set[Reactive[FullMVStruct]]) = {
    val firstFrameTurn = synchronized {_versions(firstFrame).txn}
    assert(firstFrameTurn == turn, s"Turn $turn called reevIn, but Turn $firstFrameTurn is first frame owner")
    (latestValue, incomings)
  }

  /**
    * progress [[firstFrame]] forward until a [[Version.isWrite]] is encountered, and
    * return the resulting notification out (with reframing if subsequent write is found).
    */
  def reevOut(turn: FullMVTurn, maybeValue: Option[V], maybeIncomings: Option[Set[Reactive[FullMVStruct]]]): NotificationResultAction.NotificationOutAndSuccessorOperation = synchronized {
    val version = _versions(firstFrame)
    assert(version.txn == turn, s"Turn $turn called deevDone, but Turn ${version.txn} is first frame owner")
    assert(version.value.isEmpty, s"cannot write one version twice")
    assert(version.pending == 0, s"cannot write not-ready version")
    assert(version.changed > 0, s"cannot write read-version")

    if(maybeValue.isDefined) {
      this.latestValue = maybeValue.get
      version.value = maybeValue
    }
    if(maybeIncomings.isDefined) {
      this.incomings = maybeIncomings.get
    }
    version.changed = 0

    progressToNextWriteForNotification(version.out)
  }

  /**
    * progresses [[firstFrame]] forward until a [[Version.isWrite]] is encountered (which is implied not
    * [[Version.isWritten]] due to being positioned at/behind [[firstFrame]]) and assemble all necessary
    * information to send out change/nochange notifications for the given FullMVTurn
    *. Also capture synchronized,
    * whether or not the possibly encountered write [[Version.isReadyForReevaluation]].
    * @return the notification and next reevaluation descriptor.
    */
  private def progressToNextWriteForNotification(out: Set[Reactive[FullMVStruct]]): NotificationResultAction.NotificationOutAndSuccessorOperation = {
    @tailrec
    def progressToNextWriteForNotification0(): NotificationResultAction.NotificationOutAndSuccessorOperation = {
      firstFrame += 1
      if (firstFrame < _versions.size) {
        val version = _versions(firstFrame)

        assert(!version.stable, s"already stable: $version")
        version.stable = true

        if (version.isWrite) {
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
    * @param txn the executing FullMVTurn
    * @return the version's position.
    */
  private def ensureReadVersion(txn: FullMVTurn, knownMinPos: Int = 0): Int = {
    val position = findOrPidgeonHole(txn, knownMinPos, knownMinPos, _versions.size)
    if(position < 0) {
      createVersion(-position, txn)
      -position
    } else {
      position
    }
  }

  def synchronizeDynamicAccess(txn: FullMVTurn): Int = synchronized {
    val position = ensureReadVersion(txn)
    val version = _versions(position)
    while(!version.stable) wait()

    val stablePosition = findOrPidgeonHole(txn, 0, 0, firstFrame)
    assert(stablePosition >= 0, "somehow, the version allocated above disappeared..")
    stablePosition
  }

  /**
    * entry point for before(this); may suspend.
    * @param txn the executing FullMVTurn
    * @return the corresponding [[Version.value]] from before this FullMVTurn, i.e., ignoring the FullMVTurn's
    *         own writes.
    */
  def dynamicBefore(txn: FullMVTurn): V = synchronized {
    before(txn, synchronizeDynamicAccess(txn))
  }

  def staticBefore(txn: FullMVTurn): V = synchronized {
    before(txn, math.abs(findOrPidgeonHole(txn, 0, 0, firstFrame)))
  }

  private def before(txn: FullMVTurn, position: Int): V = synchronized {
    assert(!valuePersistency.isTransient, "before read on transient node")
    prevWrite(txn, position).read()
  }

  /**
    * entry point for now(this); may suspend.
    * @param txn the executing FullMVTurn
    * @return the corresponding [[Version.value]] at the current point in time (but still serializable), i.e.,
    *         possibly returning the FullMVTurn
    *        's own write if this already occurred.
    */
  def dynamicNow(txn: FullMVTurn): V = synchronized {
    val position = synchronizeDynamicAccess(txn)
    nowGivenOwnVersion(txn, position)
  }

  def staticNow(txn: FullMVTurn): V = synchronized {
    val position = findOrPidgeonHole(txn, 0, 0, firstFrame)
    if(position < 0) {
      beforeOrInit(txn, -position)
    } else {
      nowGivenOwnVersion(txn, position)
    }
  }

  private def nowGivenOwnVersion(txn: FullMVTurn, position: Int): V = {
    val ownVersion = _versions(position)
    if(ownVersion.isWritten) {
      ownVersion.read()
    } else {
      beforeOrInit(txn, position)
    }
  }

  private def beforeOrInit(txn: FullMVTurn, position: Int): V = synchronized {
    if(valuePersistency.isTransient) valuePersistency.initialValue else before(txn, position)
  }

  /**
    * entry point for after(this); may suspend.
    * @param txn the executing FullMVTurn
    * @return the corresponding [[Version.value]] from after this FullMVTurn, i.e., awaiting and returning the
    *         FullMVTurn's own write if one has occurred or will occur.
    */
  def dynamicAfter(txn: FullMVTurn): V = {
    val position = synchronizeDynamicAccess(txn)
    val version = _versions(position)
    // Note: isWrite, not isWritten!
    if(version.isWrite) {
      while(!version.isWritten) {
        // TODO figure out how we want to do this
        System.err.println(s"WARNING: Suspending "+txn+" for dynamic after on its own placeholder; aside from user-specified deadlocks, this may deadlock the application from depleting the task pool of workers.")
        wait()
      }
      version.read()
    } else {
      beforeOrInit(txn, position)
    }
  }

  def staticAfter(txn: FullMVTurn): V = {
    val position = findOrPidgeonHole(txn, 0, 0, firstFrame)
    if(position < 0) {
      beforeOrInit(txn, -position)
    } else {
      val version = _versions(position)
      // Note: isWrite, not isWritten!
      if(version.isWrite) {
        version.read()
      } else {
        beforeOrInit(txn, position)
      }
    }
  }

  /**
    * entry point for reg-read(this, ticket.issuer) (i.e., read [[Version.value]] assuming edge this -> ticket.issuer exists)
    * @param txn the executing reevaluation's FullMVTurn
    *
    * @return the corresponding [[Version.value]]
    */
  def regRead(txn: FullMVTurn): V = synchronized {
    val position = findOrPidgeonHole(txn, 0, 0, firstFrame)
    if(position >= 0) {
      val thisVersion = _versions(position)
      //      assert(thisVersion.out.contains(ticket.issuer), "regRead invoked without existing edge")
      if (thisVersion.isWrite) {
        thisVersion.read()
      } else {
        prevWrite(txn, position).read()
      }
    } else {
      //      assert(_versions(-position - 1).out.contains(ticket.issuer), "regRead invoked without existing edge")
      prevWrite(txn, -position).read()
    }
  }

  // =================== DYNAMIC OPERATIONS ====================

  /**
    * entry point for discover(this, add). May suspend.
    * @param txn the executing reevaluation's FullMVTurn
    *
    * @param add the new edge's sink node
    * @return the appropriate [[Version.value]].
    */
  def discover(txn: FullMVTurn, add: Reactive[FullMVStruct]): (ArrayBuffer[FullMVTurn], Option[FullMVTurn]) = synchronized {
    val position = ensureReadVersion(txn)
    assert(!_versions(position).out.contains(add), "must not discover an already existing edge!")
    _versions(position).out += add
    retrofitSourceOuts(position, add, +1)
  }

  /**
    * entry point for drop(this, ticket.issuer); may suspend temporarily.
    * @param txn the executing reevaluation's FullMVTurn
    *
    * @param remove the removed edge's sink node
    */
  def drop(txn: FullMVTurn, remove: Reactive[FullMVStruct]): (ArrayBuffer[FullMVTurn], Option[FullMVTurn]) = synchronized {
    val position = ensureReadVersion(txn)
    assert(_versions(position).out.contains(remove), "must not drop a non-existing edge!")
    _versions(position).out -= remove
    retrofitSourceOuts(position, remove, -1)
  }

  /**
    * performs the reframings on the sink of a discover(n, this) with arity +1, or drop(n, this) with arity -1
    * @param successorWrittenVersions the reframings to perform for successor written versions
    * @param maybeSuccessorFrame maybe a reframing to perform for the first successor frame
    * @param arity +1 for discover adding frames, -1 for drop removing frames.
    */
  def retrofitSinkFrames(successorWrittenVersions: ArrayBuffer[FullMVTurn], maybeSuccessorFrame: Option[FullMVTurn], arity: Int): Unit = synchronized {
    @tailrec
    def retrofitSinkFrames0(idx: Int, minPos: Int): Unit = {
      if(idx < successorWrittenVersions.size) {
        val txn = successorWrittenVersions(idx)
        val position = ensureReadVersion(txn, minPos)
        _versions(position).changed += arity
        retrofitSinkFrames0(idx + 1, position + 1)
      } else if (maybeSuccessorFrame.isDefined) {
        val txn = maybeSuccessorFrame.get
        val position = ensureReadVersion(txn, minPos)
        _versions(position).pending += arity
      }
    }
    retrofitSinkFrames0(0, firstFrame)
  }

  /**
    * rewrites all affected [[Version.out]] of the source this during drop(this, delta) with arity -1 or
    * discover(this, delta) with arity +1, and collects FullMVTurn
    *s for retrofitting frames on the sink node
    * @param position the executing FullMVTurn
    *                's version's position
    * @param delta the outgoing dependency to add/remove
    * @param arity +1 to add, -1 to remove delta to each [[Version.out]]
    * @return a list of FullMVTurn
    *        s with written successor versions and maybe the FullMVTurn
    *        of the first successor
    *         frame if it exists, for which reframings have to be performed at the sink.
    */
  private def retrofitSourceOuts(position: Int, delta: Reactive[FullMVStruct], arity: Int): (ArrayBuffer[FullMVTurn], Option[FullMVTurn]) = {
    // allocate array to the maximum number of written versions that might follow
    // (any version at index firstFrame or later can only be a frame, not written)
    val successorWrittenVersions = new ArrayBuffer[FullMVTurn](firstFrame - position - 1)
    for(pos <- position + 1 until _versions.size) {
      val version = _versions(pos)
      if(arity < 0) version.out -= delta else version.out += delta
      // as per above, this is implied false if pos >= firstFrame:
      if(version.isWritten) successorWrittenVersions += version.txn
    }
    val maybeSuccessorFrame = if (firstFrame < _versions.size) Some(_versions(firstFrame).txn)  else None
    (successorWrittenVersions, maybeSuccessorFrame)
  }
}
