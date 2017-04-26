package rescala.fullmv


import rescala.graph.Reactive

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

sealed trait FramingBranchResult[+R] {
  def processBranching(txn: FullMVTurn, taskPool: TaskPool): Unit
}
case object FramingBranchEnd extends FramingBranchResult[Nothing] {
  override def processBranching(txn: FullMVTurn, taskPool: TaskPool): Unit = {
    txn.branches(-1)
  }
}
case class FramingBranchOut[R](out: Set[Reactive[FullMVStruct]]) extends FramingBranchResult[R] {
  override def processBranching(txn: FullMVTurn, taskPool: TaskPool): Unit = {
    if(out.size != 1) txn.branches(out.size - 1)
    out.foreach { node => taskPool.addFraming(node, txn) }
  }
}
case class FramingBranchOutSuperseding[R](out: Set[Reactive[FullMVStruct]], supersede: FullMVTurn) extends FramingBranchResult[R] {
  override def processBranching(txn: FullMVTurn, taskPool: TaskPool): Unit = {
    if(out.size != 1) txn.branches(out.size - 1)
    out.foreach { node => taskPool.addSupersedingFraming(node, txn, supersede) }
  }
}

class Version[D <: Pulse[_], R](val txn: FullMVTurn, var out: Set[Reactive[FullMVStruct]], var pending: Int, var changed: Int, var value: D) {
  def isWritten: Boolean = changed == 0 && value.isChange
  def isFrame: Boolean = pending > 0 || (changed > 0 && !isWritten)
  def isWrite: Boolean = pending > 0 || changed > 0 || isWritten
  def isReadOrDynamic: Boolean = !isWrite
  def isReadyForReevaluation: Boolean = pending == 0 && changed > 0
  def read(): D = value.get

  override def toString: String = "Version("+txn+", "+out+", pending="+pending+", changed="+changed+", "+value+")"
}

sealed trait NotificationResultAction[+R]
case object NotGlitchFreeReady extends NotificationResultAction[Nothing]
case object ResolvedQueuedToUnchanged extends NotificationResultAction[Nothing]
case object GlitchFreeReadyButQueued extends NotificationResultAction[Nothing]
case class GlitchFreeReady(txn: FullMVTurn) extends NotificationResultAction[Nothing]
case class NotificationAndMaybeNextReevaluation[R](out: Set[Reactive[FullMVStruct]], txn: FullMVTurn, changed: Boolean, maybeFollowFraming: Option[FullMVTurn], reevaluationReady: Boolean) extends NotificationResultAction[R]

class NodeVersionHistory[V](val host: Host, init: FullMVTurn, initialValue: PersistentValue[V]) extends PersistentReevaluationStruct[V, R] with PersistentAccessStruct[V, R] {
  // =================== STORAGE ====================

  var _versions = new ArrayBuffer[Version[PersistentValue[V], R]](6)
  _versions += new Version[PersistentValue[V], R](init, Set(), 0, 0, Some(initialValue))
  var latestValue: PersistentValue[V] = initialValue

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
  private def createVersion(position: Int, txn: FullMVTurn, pending: Int = 0, changed: Int = 0): Version[PersistentValue[V], R] = {
    assert(position > 0)
    val version = new Version[PersistentValue[V], R](txn, _versions(position - 1).out, pending, changed, None)
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
    *is found, it establishes an order in host.sgt against all other versions' FullMVTurn
    *s, placing
    * the given FullMVTurn
    *as late as possible. For this it first finds the latest possible insertion point. Assume,
    * for a given txn, this is between versions y and z. As the latest point is used, it is txn < z.txn. To fixate this
    * order, the search thus attempts to establish y.txn < txn in host.sgt. This may fail, however, if concurrent
    * threads intervened and established txn < y.txn. For this case, the search keeps track of the latest x for which
    * x.txn < txn is already established. It then restarts the search in the reduced fallback range between x and y.
    * @param lookFor the FullMVTurn
    *               to look for
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
      if(host.sgt.ensureOrder(_versions(math.abs(from) - 1).txn, lookFor) == FirstFirst) {
        -from
      } else {
        findOrPidgeonHole(lookFor, recoverFrom, recoverFrom, from)
      }
    } else {
      val idx = from+(to-from-1)/2
      val candidate = _versions(idx).txn
      if(candidate == lookFor) {
        idx
      } else host.sgt.getOrder(candidate, lookFor) match {
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
    *
    * @return the position (positive values) or insertion point (negative values)
    */
  private def findFrame(txn: FullMVTurn): Int = {
    findOrPidgeonHole(txn, firstFrame, firstFrame, _versions.size)
  }

  /**
    * traverse history back in time from the given position until the first write (i.e. [[Version.isWrite]])
    * @param txn the FullMVTurn
    *           for which the previous write is being searched; used for error reporting only.
    * @param from the FullMVTurn
    *            's position, search starts at this position's predecessor version and moves backwards
    * @return the first encountered version with [[Version.isWrite]]
    */
  private def prevWrite(txn: FullMVTurn, from: Int): Version[PersistentValue[V], R] = {
    var position = from - 1
    while(position >= 0 && !_versions(position).isWrite) position -= 1
    if(position < 0) throw new IllegalArgumentException(txn + " does not have a preceding write in versions " + _versions.take(from))
    _versions(position)
  }

  // =================== READ SUSPENSION INFRASTRUCTURE ====================

  private def blockUntilNoLongerFrame(txn: FullMVTurn) = {
    // parameter included to allow parking-per-frame blocking/unblocking
    // but we implement coarse parking-per-node only, so we ignore it here.
    wait()
  }

  private def frameRemoved(txn: FullMVTurn): Unit = {
    // parameter included to allow parking-per-frame blocking/unblocking
    // but we implement coarse parking-per-node only, so we ignore it here.
    notifyAll()
    // Note that, e.g., in case of [[retrofitDropFrames]], multiple frames may
    // be removed within a single synchronization scope -- these removal notifications
    // could maybe be batched in some sensible manner.
  }

  // =================== FRAMING ====================

  /**
    * entry point for superseding framing
    * @param txn the FullMVTurn
    *           visiting the node for framing
    * @param supersede the FullMVTurn
    *                 whose frame was superseded by the visiting FullMVTurn
    *                 at the previous node
    */
  def incrementSupersedeFrame(txn: FullMVTurn, supersede: FullMVTurn): Unit = {
    assert(txn.phase == Preparing)
    assert(supersede.phase == Preparing)
    synchronized {
      val position = findFrame(supersede)
      if (position >= 0) {
        _versions(position).pending -= 1
        if(_versions(position).isReadOrDynamic) frameRemoved(txn)
      } else {
        createVersion(-position, supersede, pending = -1)
      }
      incrementFrame0(txn)
    }.processBranching(txn, host.taskPool)
  }

  /**
    * entry point for regular framing
    * @param txn the FullMVTurn
    *           visiting the node for framing
    */
  def incrementFrame(txn: FullMVTurn): Unit = {
    assert(txn.phase == Preparing)
    synchronized { incrementFrame0(txn) }.processBranching(txn, host.taskPool)
  }

  /**
    * creates a frame if none exists, or increments an existing frame's [[Version.pending]] counter.
    * @param txn the FullMVTurn
    *           visiting the node for framing
    * @return a descriptor of how this framing has to propagate
    */
  private def incrementFrame0(txn: FullMVTurn): FramingBranchResult[R] = {
    val position = findFrame(txn)
    if(position >= 0) {
      _versions(position).pending += 1
      FramingBranchEnd
    } else {
      val frame = createVersion(-position, txn, pending = 1)
      val previousFirstFrame = firstFrame
      if(-position < firstFrame) {
        firstFrame = -position
      }
      if(frame.out.isEmpty) {
        FramingBranchEnd
      } else if(-position < previousFirstFrame) {
        if(previousFirstFrame < _versions.size) {
          val supersede = _versions(previousFirstFrame).txn
          FramingBranchOutSuperseding(frame.out, supersede)
        } else {
          FramingBranchOut(frame.out)
        }
      } else {
        FramingBranchEnd
      }
    }
  }

  /*
   * =================== NOTIFICATIONS/ / REEVALUATION ====================
   */

  /**
    * entry point for change/nochange notification reception
    * @param txn the FullMVTurn
    *           sending the notification
    * @param changed whether or not the dependency changed
    * @param maybeFollowFrame possibly a FullMVTurn
    *                        for which to create a subsequent frame, furthering its partial framing.
    */
  def notify(txn: FullMVTurn, changed: Boolean, maybeFollowFrame: Option[FullMVTurn]): NotificationResultAction[R] = synchronized {
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
          GlitchFreeReady(version.txn)
        } else {
          // ResolvedFirstFrameToUnchanged
          firstFrame += 1
          frameRemoved(txn)
          progressToNextWriteForNotification(version.out, version.txn, changed = false)
        }
      } else {
        if(version.changed > 0) {
          GlitchFreeReadyButQueued
        } else {
          ResolvedQueuedToUnchanged
        }
      }
    } else {
      NotGlitchFreeReady
    }
    /* TODO move to node
    // enact subsequent action
    nextStep match {
      case GlitchFreeReadyButQueued =>
        // do nothing
      case ResolvedQueuedToUnchanged =>
        txn.branches(-1)
      case NotGlitchFreeReady =>
        txn.branches(-1)
      case ready: GlitchFreeReady[V] =>
        progressReevaluation(ready.version)
      case notification: NotificationAndMaybeNextReevaluation[V] =>
        notification.send(host.taskPool)
        if (notification.maybeNextReevaluation.isDefined) {
          progressReevaluation(notification.maybeNextReevaluation.get)
        }
    }
    */
  }

  override def reevIn(turn: Turn[_]): (PersistentValue[V], Set[Reactive[FullMVStruct]]) = {
    assert(synchronized{_versions(firstFrame)}.txn == turn)
    (latestValue, incomings)
  }

  override def reevDone(turn: Turn[_], maybeValue: PersistentValue[V], incomings: Set[Reactive[FullMVStruct]]): Unit = {
    assert(synchronized{_versions(firstFrame)}.txn == turn)
    if(maybeValue.isChange) {
      this.latestValue = maybeValue.get
      _versions(firstFrame).value = maybeValue
    }
    this.incomings = incomings
  }

  /**
    * progress [[firstFrame]] forward until a [[Version.isWrite]] is encountered, and
    * return the resulting notification (with reframing if subsequent write is found).
    * Also include, whether the the new [[firstFrame]] is [[Version.isReadyForReevaluation]]
    */
  def reevOut(turn: Turn[_]): NotificationAndMaybeNextReevaluation[R] = synchronized {
    val version = _versions(firstFrame)
    assert(version.txn == turn)
    version.changed = 0
    firstFrame += 1
    frameRemoved(version.txn)
    progressToNextWriteForNotification(version.out, version.txn, version.value.isDefined)

    /* TODO move to node
      // send out notifications
      if(out.size != 1) txn.branches(out.size - 1)
      for (node <- out) taskPool.addNotification(node, txn, changed, maybeFollowFraming)

      // progress with next reevaluation if found
      if (notification.maybeNextReevaluation.isDefined) {
        progressReevaluation(notification.maybeNextReevaluation.get)
      }
   */
  }

  /**
    * progresses [[firstFrame]] forward until a [[Version.isWrite]] is encountered (which is implied not
    * [[Version.isWritten]] due to being positioned at/behind [[firstFrame]]) and assemble all necessary
    * information to send out change/nochange notifications for the given FullMVTurn
    *. Also capture synchronized,
    * whether or not the possibly encountered write [[Version.isReadyForReevaluation]].
    * @param out the outgoing dependencies to notify
    * @param txn the FullMVTurn
    *           that notifies
    * @param changed change or nochange
    * @return the notification and next reevaluation descriptor.
    */
  @tailrec
  private def progressToNextWriteForNotification(out: Set[Reactive[FullMVStruct]], txn: FullMVTurn, changed: Boolean): NotificationAndMaybeNextReevaluation[R] = {
    if(firstFrame < _versions.size) {
      val version = _versions(firstFrame)
      if(version.isWrite) {
        NotificationAndMaybeNextReevaluation[R](out, txn, changed, Some(version.txn), version.isReadyForReevaluation)
      } else {
        firstFrame += 1
        progressToNextWriteForNotification(out, txn, changed)
      }
    } else {
      NotificationAndMaybeNextReevaluation[R](out, txn, changed, None, false)
    }
  }

  // =================== READ OPERATIONS ====================

  /**
    * ensures at least a read version is stored to track executed reads or dynamic operations.
    * @param txn the executing FullMVTurn
    *
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

  /**
    * entry point for before(this); may suspend.
    * @param txn the executing FullMVTurn
    *
    * @return the corresponding [[Version.value]] from before this FullMVTurn
    *        , i.e., ignoring the FullMVTurn
    *        's
    *         own writes.
    */
  def before(txn: FullMVTurn): PersistentValue[V] = synchronized {
    @tailrec
    def before0(): PersistentValue[V] = {
      val thisPosition = ensureReadVersion(txn)
      val readFrom = prevWrite(txn, thisPosition)
      if(readFrom.isWritten) {
        readFrom.read()
      } else {
        blockUntilNoLongerFrame(readFrom.txn)
        before0()
      }
    }
    before0()
  }

  /**
    * entry point for now(this); may suspend.
    * @param txn the executing FullMVTurn
    *
    * @return the corresponding [[Version.value]] at the current point in time (but still serializable), i.e.,
    *         possibly returning the FullMVTurn
    *        's own write if this already occurred.
    */
  def now(txn: FullMVTurn): PersistentValue[V] = synchronized {
    val position = ensureReadVersion(txn)
    if(_versions(position).isWritten) {
      _versions(position).read()
    } else {
      before(txn)
    }
  }

  /**
    * entry point for after(this); may suspend.
    * @param txn the executing FullMVTurn
    *
    * @return the corresponding [[Version.value]] from after this FullMVTurn
    *        , i.e., awaiting and returning the
    *         FullMVTurn
    *        's own write if one has occurred or will occur.
    */
  def after(txn: FullMVTurn): PersistentValue[V] = synchronized {
    @tailrec
    def after0(): PersistentValue[V] = {
      val thisPosition = ensureReadVersion(txn)
      val thisVersion = _versions(thisPosition)
      if(thisVersion.isWritten) {
        thisVersion.read()
      } else if (thisVersion.isWrite) {
        blockUntilNoLongerFrame(thisVersion.txn)
        after0()
      } else {
        val prevReev = prevWrite(txn, thisPosition)
        if (prevReev.isWritten) {
          prevReev.read()
        } else {
          blockUntilNoLongerFrame(prevReev.txn)
          after0()
        }
      }
    }
    after0()
  }

  /**
    * entry point for reg-read(this, ticket.issuer) (i.e., read [[Version.value]] assuming edge this -> ticket.issuer exists)
    * @param txn the executing reevaluation's FullMVTurn
    *
    * @return the corresponding [[Version.value]]
    */
  def regRead(txn: FullMVTurn): PersistentValue[V] = synchronized {
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
  def discover(txn: FullMVTurn, add: Reactive[FullMVStruct]): PersistentValue[V] = {
    val (result, (successorWrittenVersions, maybeSuccessorFrame)) = synchronized {
      val position = ensureReadVersion(txn)
      assert(!_versions(position).out.contains(add), "must not discover an already existing edge!")
      val result = after(txn)
      _versions(position).out += add
      val rewrite = retrofitSourceOuts(position, add, +1)
      (result, rewrite)
    }
    add.struct.retrofitSinkFrames(successorWrittenVersions, maybeSuccessorFrame, +1)
    result
  }

  /**
    * entry point for drop(this, ticket.issuer); may suspend temporarily.
    * @param txn the executing reevaluation's FullMVTurn
    *
    * @param remove the removed edge's sink node
    */
  def drop(txn: FullMVTurn, remove: Reactive[FullMVStruct]): Unit = {
    val (successorWrittenVersions, maybeSuccessorFrame) = synchronized {
      val position = ensureReadVersion(txn)
      assert(_versions(position).out.contains(remove), "must not drop a non-existing edge!")
      _versions(position).out -= remove
      retrofitSourceOuts(position, remove, -1)
    }
    remove.retrofitSinkFrames(successorWrittenVersions, maybeSuccessorFrame, -1)
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
        if(arity < 0 && _versions(position).isReadOrDynamic) frameRemoved(txn)
        retrofitSinkFrames0(idx + 1, position + 1)
      } else if (maybeSuccessorFrame.isDefined) {
        val txn = maybeSuccessorFrame.get
        val position = ensureReadVersion(txn, minPos)
        _versions(position).pending += arity
        if(arity < 0 && _versions(position).isReadOrDynamic) frameRemoved(txn)
      }
    }
    retrofitSinkFrames0(0, firstFrame + 1)
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
