package rescala.fullmv.api

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.existentials

object SignalVersionList {
  type O = SignalVersionList[_]
}

import SignalVersionList.O

sealed trait FramingBranchResult {
  def processBranching(txn: Transaction, taskPool: TaskPool): Unit
}
case object FramingBranchEnd extends FramingBranchResult {
  override def processBranching(txn: Transaction, taskPool: TaskPool): Unit = {
    txn.branches(-1)
  }
}
case class FramingBranchOut(out: Set[O]) extends FramingBranchResult {
  override def processBranching(txn: Transaction, taskPool: TaskPool): Unit = {
    if(out.size != 1) txn.branches(out.size - 1)
    out.foreach { node => taskPool.addFraming(node, txn) }
  }
}
case class FramingBranchOutSuperseding(out: Set[O], supersede: Transaction) extends FramingBranchResult {
  override def processBranching(txn: Transaction, taskPool: TaskPool): Unit = {
    if(out.size != 1) txn.branches(out.size - 1)
    out.foreach { node => taskPool.addSupersedingFraming(node, txn, supersede) }
  }
}

class Version[V](val txn: Transaction, var out: Set[O], var pending: Int, var changed: Int, var value: Option[V]) {
  def isWritten: Boolean = value.isDefined
  def isFrame: Boolean = pending > 0 || (changed > 0 && !isWritten)
  def isWrite: Boolean = pending > 0 || changed > 0 || isWritten
  def isReadOrDynamic: Boolean = !isWrite
  def isReadyForReevaluation: Boolean = pending == 0 && changed > 0
  def read(): V = value.get

  override def toString: String = "Version("+txn+", "+out+", pending="+pending+", changed="+changed+", "+value+")"
}

sealed trait NotificationResultAction[+V]
case object NotGlitchFreeReady extends NotificationResultAction[Nothing]
case object ResolvedQueuedToUnchanged extends NotificationResultAction[Nothing]
case object GlitchFreeReadyButQueued extends NotificationResultAction[Nothing]
case class GlitchFreeReady[V](version: Version[V]) extends NotificationResultAction[V]
case class NotificationAndMaybeNextReevaluation[V](out: Set[O], txn: Transaction, changed: Boolean, maybeFollowFraming: Option[Transaction], maybeNextReevaluation: Option[Version[V]]) extends NotificationResultAction[V] {
  def send(taskPool: TaskPool): Unit = {
    if(out.size != 1) txn.branches(out.size - 1)
    for (node <- out) taskPool.addNotification(node, txn, changed, maybeFollowFraming)
  }
}

case class ReevaluationTicket(txn: Transaction, issuer: O)
class SignalVersionList[V](val host: Host, init: Transaction, initialValue: V, val userComputation: (ReevaluationTicket, V) => V) {
  // =================== STORAGE ====================

  var _versions = new ArrayBuffer[Version[V]](6)
  _versions += new Version[V](init, Set(), 0, 0, Some(initialValue))
  var latestValue: V = initialValue

  /**
    * creates a version and inserts it at the given position; default parameters create a "read" version
    * @param position the position
    * @param txn the transaction this version is associated with
    * @param pending the number of pending notifications, none by default
    * @param changed the number of already received change notifications, none by default
    * @return the created version
    */
  def createVersion(position: Int, txn: Transaction, pending: Int = 0, changed: Int = 0): Version[V] = {
    assert(position > 0)
    val version = new Version[V](txn, _versions(position - 1).out, pending, changed, None)
    _versions.insert(position, version)
    if(position <= firstFrame) firstFrame += 1
    version
  }


  // =================== NAVIGATION ====================
  var firstFrame: Int = _versions.size

  /**
    * performs binary search for the given transaction in _versions. If no version associated with the given
    * transaction is found, it establishes an order in host.sgt against all other versions' transactions, placing
    * the given transaction as late as possible. For this it first finds the latest possible insertion point. Assume,
    * for a given txn, this is between versions y and z. As the latest point is used, it is txn < z.txn. To fixate this
    * order, the search thus attempts to establish y.txn < txn in host.sgt. This may fail, however, if concurrent
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
  private def findOrPidgeonHole(lookFor: Transaction, recoverFrom: Int, from: Int, to: Int): Int = {
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
    * determine the position or insertion point for a framing transaction
    * @param txn the transaction
    * @return the position (positive values) or insertion point (negative values)
    */
  private def findFrame(txn: Transaction): Int = {
    findOrPidgeonHole(txn, firstFrame, firstFrame, _versions.size)
  }

  /**
    * traverse history back in time from the given position until the first write (i.e. [[Version.isWrite]])
    * @param txn the transaction for which the previous write is being searched; used for error reporting only.
    * @param from the transaction's position, search starts at this position's predecessor version and moves backwards
    * @return the first encountered version with [[Version.isWrite]]
    */
  private def prevWrite(txn: Transaction, from: Int): Version[V] = {
    var position = from - 1
    while(position >= 0 && !_versions(position).isWrite) position -= 1
    if(position < 0) throw new IllegalArgumentException(txn + " does not have a preceding write in versions " + _versions.take(from))
    _versions(position)
  }

  // =================== READ SUSPENSION INFRASTRUCTURE ====================

  private def blockUntilNoLongerFrame(txn: Transaction) = {
    // parameter included to allow parking-per-frame blocking/unblocking
    // but we implement coarse parking-per-node only, so we ignore it here.
    wait()
  }

  private def frameRemoved(txn: Transaction): Unit = {
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
    * @param txn the transaction visiting the node for framing
    * @param supersede the transaction whose frame was superseded by the visiting transaction at the previous node
    */
  def incrementSupersedeFrame(txn: Transaction, supersede: Transaction): Unit = {
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
    * @param txn the transaction visiting the node for framing
    */
  def incrementFrame(txn: Transaction): Unit = {
    assert(txn.phase == Preparing)
    synchronized { incrementFrame0(txn) }.processBranching(txn, host.taskPool)
  }

  /**
    * creates a frame if none exists, or increments an existing frame's [[Version.pending]] counter.
    * @param txn the transaction visiting the node for framing
    * @return a descriptor of how this framing has to propagate
    */
  private def incrementFrame0(txn: Transaction): FramingBranchResult = {
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
    * @param txn the transaction sending the notification
    * @param changed whether or not the dependency changed
    * @param maybeFollowFrame possibly a transaction for which to create a subsequent frame, furthering its partial framing.
    */
  def notify(txn: Transaction, changed: Boolean, maybeFollowFrame: Option[Transaction]): Unit = {
    val nextStep: NotificationResultAction[V] = synchronized {
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
            GlitchFreeReady(version)
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
    }

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
  }

  /**
    * executes a reevaluation, progress [[firstFrame]] forward until a [[Version.isWrite]] is encountered, and
    * send the resulting notifications (with reframing if subsequent write is found). Also execute the subsequent
    * ready reevaluations if the new [[firstFrame]] is [[Version.isReadyForReevaluation]]
    * @param version the version ready for reevaluation
    */
  @tailrec
  private def progressReevaluation(version: Version[V]): Unit = {
    // do reevaluation
    val newValue = userComputation(ReevaluationTicket(version.txn, this), latestValue)
    val selfChanged = latestValue != newValue

    // update version and search for successor framing and possibly reevaluation
    val notification = synchronized {
      version.changed = 0
      if (selfChanged) {
        version.value = Some(newValue)
        latestValue = newValue
      }
      firstFrame += 1
      frameRemoved(version.txn)
      progressToNextWriteForNotification(version.out, version.txn, selfChanged)
    }

    // send out notifications
    notification.send(host.taskPool)

    // progress with next reevaluation if found
    if (notification.maybeNextReevaluation.isDefined) {
      progressReevaluation(notification.maybeNextReevaluation.get)
    }
  }

  /**
    * progresses [[firstFrame]] forward until a [[Version.isWrite]] is encountered (which is implied not
    * [[Version.isWritten]] due to being positioned at/behind [[firstFrame]]) and assemble all necessary
    * information to send out change/nochange notifications for the given transaction. Also capture synchronized,
    * whether or not the possibly encountered write [[Version.isReadyForReevaluation]].
    * @param out the outgoing dependencies to notify
    * @param txn the transaction that notifies
    * @param changed change or nochange
    * @return the notification and next reevaluation descriptor.
    */
  @tailrec
  private def progressToNextWriteForNotification(out: Set[O], txn: Transaction, changed: Boolean): NotificationAndMaybeNextReevaluation[V] = {
    if(firstFrame < _versions.size) {
      val version = _versions(firstFrame)
      if(version.isWrite) {
        NotificationAndMaybeNextReevaluation[V](out, txn, changed, Some(version.txn), if(version.isReadyForReevaluation) Some(version) else None)
      } else {
        firstFrame += 1
        progressToNextWriteForNotification(out, txn, changed)
      }
    } else {
      NotificationAndMaybeNextReevaluation[V](out, txn, changed, None, None)
    }
  }

  // =================== READ OPERATIONS ====================

  /**
    * ensures at least a read version is stored to track executed reads or dynamic operations.
    * @param txn the executing transaction
    * @return the version's position.
    */
  private def ensureReadVersion(txn: Transaction, knownMinPos: Int = 0): Int = {
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
    * @param txn the executing transaction
    * @return the corresponding [[Version.value]] from before this transaction, i.e., ignoring the transaction's
    *         own writes.
    */
  def before(txn: Transaction): V = synchronized {
    @tailrec
    def before0(): V = {
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
    * @param txn the executing transaction
    * @return the corresponding [[Version.value]] at the current point in time (but still serializable), i.e.,
    *         possibly returning the transaction's own write if this already occurred.
    */
  def now(txn: Transaction): V = synchronized {
    val position = ensureReadVersion(txn)
    if(_versions(position).isWritten) {
      _versions(position).read()
    } else {
      before(txn)
    }
  }

  /**
    * entry point for after(this); may suspend.
    * @param txn the executing transaction
    * @return the corresponding [[Version.value]] from after this transaction, i.e., awaiting and returning the
    *         transaction's own write if one has occurred or will occur.
    */
  def after(txn: Transaction): V = synchronized {
    @tailrec
    def after0(): V = {
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
    * @param ticket the executing reevaluation's ticket
    * @return the corresponding [[Version.value]]
    */
  def regRead(ticket: ReevaluationTicket): V = synchronized {
    val txn = ticket.txn
    val position = findOrPidgeonHole(txn, 0, 0, firstFrame)
    if(position >= 0) {
      val thisVersion = _versions(position)
      assert(thisVersion.out.contains(ticket.issuer), "regRead invoked without existing edge")
      if (thisVersion.isWrite) {
        thisVersion.read()
      } else {
        prevWrite(txn, position).read()
      }
    } else {
      assert(_versions(-position - 1).out.contains(ticket.issuer), "regRead invoked without existing edge")
      prevWrite(txn, -position).read()
    }
  }

  // =================== DYNAMIC OPERATIONS ====================

  /**
    * entry point for discover(this, ticket.issuer). May suspend.
    * @param ticket the executing reevaluation's ticket
    * @return the appropriate [[Version.value]].
    */
  def discoverSuspend(ticket: ReevaluationTicket): V = {
    val txn = ticket.txn
    val add = ticket.issuer
    val (result, (successorWrittenVersions, maybeSuccessorFrame)) = synchronized {
      val position = ensureReadVersion(txn)
      assert(!_versions(position).out.contains(add), "must not discover an already existing edge!")
      val result = after(txn)
      _versions(position).out += add
      val rewrite = retrofitSourceOuts(position, add, +1)
      (result, rewrite)
    }
    add.retrofitSinkFrames(successorWrittenVersions, maybeSuccessorFrame, +1)
    result
  }

  /**
    * entry point for drop(this, ticket.issuer); may suspend temporarily.
    * @param ticket the executing reevaluation's ticket
    */
  def drop(ticket: ReevaluationTicket): Unit = {
    val txn = ticket.txn
    val remove = ticket.issuer
    val (successorWrittenVersions, maybeSuccessorFrame) = synchronized {
      val position = ensureReadVersion(txn)
      assert(_versions(position).out.contains(remove), "must not drop a non-existing edge!")
      val result = after(txn) // TODO technically unnecessary, think about if removing it violates anything.
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
  def retrofitSinkFrames(successorWrittenVersions: ArrayBuffer[Transaction], maybeSuccessorFrame: Option[Transaction], arity: Int): Unit = synchronized {
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
    * discover(this, delta) with arity +1, and collects transactions for retrofitting frames on the sink node
    * @param position the executing transaction's version's position
    * @param delta the outgoing dependency to add/remove
    * @param arity +1 to add, -1 to remove delta to each [[Version.out]]
    * @return a list of transactions with written successor versions and maybe the transaction of the first successor
    *         frame if it exists, for which reframings have to be performed at the sink.
    */
  private def retrofitSourceOuts(position: Int, delta: O, arity: Int): (ArrayBuffer[Transaction], Option[Transaction]) = {
    // allocate array to the maximum number of written versions that might follow
    // (any version at index firstFrame or later can only be a frame, not written)
    val successorWrittenVersions = new ArrayBuffer[Transaction](firstFrame - position - 1)
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
