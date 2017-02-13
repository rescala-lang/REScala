package rescala.fullmv.api

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


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
    txn.branches(out.size - 1)
    out.foreach { node => taskPool.addFraming(node, txn) }
  }
}
case class FramingBranchOutSuperseding(out: Set[O], supersede: Transaction) extends FramingBranchResult {
  override def processBranching(txn: Transaction, taskPool: TaskPool): Unit = {
    txn.branches(out.size - 1)
    out.foreach { node => taskPool.addSupersedingFraming(node, txn, supersede) }
  }
}

final class RewriteFramesBuffer(maxElementConut: Int) {
  val txns: Array[Transaction] = new Array[Transaction](maxElementConut)
  var written: Int = 0
  var framed: Int = 0

  def addWritten(txn: Transaction): Unit = {
    assert(framed == 0, "Shouldn't have written versions after frames!")
    txns(written) = txn
    written += 1
  }

  def addFramed(txn: Transaction): Unit = {
    txns(written + framed) = txn
    framed += 1
  }

  def size: Int = written + framed
  def apply(idx: Int): Transaction = txns(idx)
  def isWritten(idx: Int): Boolean = idx < written
}

class SignalVersionList[V](val host: Host, init: Transaction, initialValue: V, val userComputation: (Transaction, V) => V) {
  // =================== STORAGE ====================
  class Version(val txn: Transaction, var out: Set[O], var pending: Int, var changed: Int, var value: Option[V]) {
    def isWritten: Boolean = value.isDefined
    def isFrame: Boolean = pending > 0 || (changed > 0 && !isWritten)
    def isWrite: Boolean = pending > 0 || changed > 0
    def isReadOrDynamic: Boolean = !isWrite
    def isReadyForReevaluation: Boolean = pending == 0 && changed > 0 && !isWritten
    def read(): V = value.get
  }

  var _versions = new ArrayBuffer[Version](6)
  _versions += new Version(init, Set(), 0, 0, Some(initialValue))
  var latestValue: V = initialValue

  def replace(pos: Int, version: Version): Unit = {
    assert(_versions(pos).txn == version.txn)
    _versions(pos) = version
  }
  def insert(pos: Int, version: Version): Unit = {
    assert(pos > 0)
    _versions.insert(pos, version)
    if(pos <= firstFrame) firstFrame += 1
  }


  // =================== NAVIGATION ====================
  var firstFrame: Int = -1

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
      if(host.sgt.requireOrder(_versions(math.abs(from) - 1).txn, lookFor) == FirstFirst) {
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
        case Some(FirstFirst) =>
          findOrPidgeonHole(lookFor, idx + 1, idx + 1, to)
        case Some(SecondFirst) =>
          findOrPidgeonHole(lookFor, recoverFrom, from, idx)
        case None =>
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
    * determine the position or insertion point for an operation known to be ordered into executed versions.
    * @param txn the transaction
    * @return the position (positive values) or insertion point (negative values)
    */
  private def findExecuted(txn: Transaction): Int = {
    findOrPidgeonHole(txn, 0, 0, firstFrame)
  }

  /**
    * determine the position or insertion point for an operation without additional knowledge
    * @param txn the transaction
    * @return the position (positive values) or insertion point (negative values)
    */
  private def find(txn: Transaction): Int = {
    findOrPidgeonHole(txn, 0, 0, _versions.size)
  }

  private def prevWrite(from: Int): Version = {
    var position = from - 1
    while(position >= 0 && !_versions(position).isWrite) position -= 1
    if(position < 0) throw new IllegalArgumentException("Does not have a preceding Reevaluation: "+_versions(from))
    _versions(position)
  }

//  private def nextReevPosition(from: Int): Option[Int] = {
//    var nextReevPosition = from + 1
//    while(nextReevPosition < _versions.size && !_versions(nextReevPosition).isWrite) nextReevPosition += 1
//    if(nextReevPosition < _versions.size) {
//      Some(nextReevPosition)
//    } else {
//      None
//    }
//  }

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

  def incrementSupersedeFrame(txn: Transaction, supersede: Transaction): Unit = {
    synchronized {
      val position = findFrame(supersede)
      if (position >= 0) {
        _versions(position).pending -= 1
        if(_versions(position).isReadOrDynamic) frameRemoved(txn)
      } else {
        val frame = new Version(supersede, _versions(-position - 1).out, pending = -1, changed = 0, None)
        insert(-position, frame)
      }
      incrementFrame0(txn)
    }.processBranching(txn, host.taskPool)
  }

  def incrementFrame(txn: Transaction): Unit = {
    synchronized { incrementFrame0(txn) }.processBranching(txn, host.taskPool)
  }

  private def incrementFrame0(txn: Transaction): FramingBranchResult = {
    val position = findFrame(txn)
    if(position >= 0) {
      _versions(position).pending += 1
      FramingBranchEnd
    } else {
      val frame = new Version(txn, _versions(-position - 1).out, pending = 1, changed = 0, None)
      insert(-position, frame)
      if(frame.out.isEmpty) {
        FramingBranchEnd
      } else if(-position < firstFrame) {
        if(firstFrame < _versions.size) {
          val supersede = _versions(firstFrame).txn
          firstFrame = -position
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
  sealed trait NotificationResultAction
  case object NoOp extends NotificationResultAction
  case class Reevaluation(version: Version) extends NotificationResultAction
  case class NotificationAndMaybeNextReevaluation(out: Set[O], txn: Transaction, changed: Boolean, maybeFollowFraming: Option[Transaction], maybeNextReevaluation: Option[Version]) extends NotificationResultAction {
    def sendAndGetNextReevaluation(taskPool: TaskPool): Unit = {
      if (changed) {
        for (node <- out) taskPool.addChangeNotification(node, txn, maybeFollowFraming)
      } else {
        for (node <- out) taskPool.addNoChangeNotification(node, txn, maybeFollowFraming)
      }
    }
  }

  def notify(txn: Transaction, changed: Boolean, maybeFollowFrame: Option[Transaction]): Unit = {
    synchronized {
      if(maybeFollowFrame.isDefined) {
        incrementFrame0(maybeFollowFrame.get)
        // ignore return value for branching out!
      }

      val position = findFrame(txn)
      val version = _versions(position)
      version.pending -= 1
      if(changed) version.changed += 1
      if(position == firstFrame) {
        if(version.pending == 0) {
          if(version.changed > 0) {
            Reevaluation(version)
          } else {
            firstFrame += 1
            frameRemoved(txn)
            progressToNextWriteForNotification(version.out, version.txn, changed = false)
          }
        } else {
          NoOp
        }
      }
    } match {
      case Reevaluation(version) =>
        progressReevaluation(version)
      case notification: NotificationAndMaybeNextReevaluation =>
        notification.sendAndGetNextReevaluation(host.taskPool)
        if (notification.maybeNextReevaluation.isDefined) {
          progressReevaluation(notification.maybeNextReevaluation.get)
        }
    }
  }

  @tailrec
  private def progressReevaluation(version: Version): Unit = {
    // do reevaluation
    val newValue = userComputation(version.txn, latestValue)
    val selfChanged = latestValue == newValue

    // update version and search for successor write
    val notification = synchronized {
      frameRemoved(version.txn)
      if (selfChanged) {
        latestValue = newValue
        version.changed = 0
      } else {
        version.value = Some(latestValue)
      }
      firstFrame += 1
      progressToNextWriteForNotification(version.out, version.txn, selfChanged)
    }

    notification.sendAndGetNextReevaluation(host.taskPool)

    // progress to next reevaluation
    if (notification.maybeNextReevaluation.isDefined) {
      progressReevaluation(notification.maybeNextReevaluation.get)
    }
  }

  @tailrec
  private def progressToNextWriteForNotification(out: Set[O], txn: Transaction, changed: Boolean): NotificationAndMaybeNextReevaluation = {
    if(firstFrame < _versions.size) {
      val version = _versions(firstFrame)
      if(version.isWrite) {
        NotificationAndMaybeNextReevaluation(out, txn, changed, Some(version.txn), if(version.isReadyForReevaluation) Some(version) else None)
      } else {
        firstFrame += 1
        progressToNextWriteForNotification(out, txn, changed)
      }
    } else {
      NotificationAndMaybeNextReevaluation(out, txn, changed, None, None)
    }
  }

//  /*
//   * Note: This is a big pile of mutually recursive methods.
//   * Upon any received change or no-change notification, this stack iterates through the version list
//   * and executes all reevaluations that are not missing further notifications from other nodes.
//   * The trampoline class is used to enable tail call optimization.
//   * The control flow alternates between holding the monitor to count the notification and look up reevaluation input,
//   * releasing the monitor to execute the reevaluation's user computation, and again holding the monitor to process
//   * the reevaluation result and search for and update the pipeline-successive reevaluation before releasing it
//   * again for executing the user computation again.
//   * For a visualization of this control flow, consult the following diagram:
//   * "doc/single node reevaluation pipeline mutual trampoline tail recursion.graphml"
//   */
//
//  /**
//    * Updates active transaction branches and notify suspended reads after a completed reevaluation.
//    * Tail-recursively traverses the version list onwards, looking for a pipeline-successive reevaluation.
//    * @param position the version's position
//    * @return Some(txn) to start a pipeline-successive reevaluation,
//    *         or None if the pipeline-successor reevaluation is not ready or does not exists.
//    */
//  private def reevDone(position: Int): Trampoline[Option[Transaction]] = {
//    val version = _versions(position)
//    version.txn.branches(version.out.size - 1)
//    frameRemoved(version.txn)
//    nextReevPosition(position) match {
//      case Some(next) => Recurse({ () => notifyUnchanged(next)})
//      case None => Result(None)
//    }
//  }
//
//  /**
//    * Turns a frame into a read version after the frame was reevaluated as unchanged.
//    * Tail-recursively traverses the version list onwards, looking for a pipeline-successive reevaluation.
//    * @param position the version's position
//    * @return Some(txn) to start a pipeline-successive reevaluation,
//    *         or None if the next reevaluation is not ready or no next reevaluation exists.
//    */
//  private def reevUnchanged(position: Int): Trampoline[Option[Transaction]] = {
//    val frame = _versions(position)
//    _versions(position) = new ReadVersion(frame.txn, frame.out)
//    frame.out.foreach{ succ => host.taskPool.addNoChangeNotification(frame.txn, succ) }
//    reevDone(position)
//  }
//
//  /**
//    * Turns a frame into a written version after the frame was reevaluated as changed.
//    * Tail-recursively traverses the version list onwards, looking for a pipeline-successive reevaluation.
//    * @param position the version's position
//    * @param value the new value
//    * @return Some(txn) to start a pipeline-successive reevaluation,
//    *         or None if the pipeline-successor reevaluation is not ready or does not exists.
//    */
//  private def reevChanged(position: Int, value: V): Trampoline[Option[Transaction]] = {
//    val frame = _versions(position)
//    _versions(position) = new Written(frame.txn, frame.out, value)
//    latestValue = value
//    frame.out.foreach{ succ => host.taskPool.addChangeNotification(frame.txn, succ) }
//    reevDone(position)
//  }
//
//  /**
//    * Delivers a no-change to the frame at the given position, either from an external [[notifyUnchanged()]] message
//    * or from a pipeline-predecessor reevaluation having completed.
//    * Tail-recursively traverses the version list onwards, looking for a pipeline-successive reevaluation.
//    * @param position the version's position
//    * @return Some(txn) to start a pipeline-successive reevaluation,
//    *         or None if the pipeline-successor reevaluation is not ready or does not exists.
//    */
//  private def notifyUnchanged(position: Int): Trampoline[Option[Transaction]] = {
//    _versions(position) match {
//      case frame: Frame if (frame.pending > 0) =>
//        frame.pending -= 1
//        if (frame.pending == 0) {
//          if (frame.changed > 0) {
//            Result(Some(frame.txn))
//          } else {
//            Recurse({ () => reevUnchanged(position)})
//          }
//        } else {
//          frame.txn.branches(-1)
//          Result(None)
//        }
//      case version =>
//        throw new IllegalStateException("Cannot process no-change notification - not a Frame: " + version)
//    }
//  }
//
//  /**
//    * If the corresponding input is given, executes the reevaluation and tail-recursively all pipeline-successive
//    * reevaluations that are ready. Note that the user computation runs without holding the node's monitor!
//    * @param maybeTransaction Possibly a transaction to start a reevaluation.
//    */
//  @tailrec
//  private def maybeUserCompAndReevOut(maybeTransaction: Option[Transaction]): Unit = {
//    if(maybeTransaction.isDefined) {
//      val txn = maybeTransaction.get
//      val v_out = userComputation(latestValue)
//      val maybeNextTransaction = if (latestValue == v_out) synchronized {
//        position(txn) match {
//          case Found(position) =>
//            reevUnchanged(position).bounce
//          case _ =>
//            throw new AssertionError("Cannot reevOutUnchanged - Frame was deleted during userComputation for " + txn)
//        }
//      } else synchronized {
//        position(txn) match {
//          case Found(position) =>
//            reevChanged(position, v_out).bounce
//          case _ =>
//            throw new AssertionError("Cannot reevOutChanged - Frame was deleted during userComputation for " + txn)
//        }
//      }
//      maybeUserCompAndReevOut(maybeNextTransaction)
//    }
//  }
//
//  /**
//    * receive an external no-change notification within the given transaction. Executes all pipelined reevaluations
//    * of this node that become ready due to this notification. Notifications (change and no-change) for other nodes
//    * spawned by any of these reevaluations are queued in the [[Host.taskPool]] of [[host]] for concurrent processing.
//    * @param txn the transaction
//    */
//  def notifyUnchanged(txn: Transaction): Unit = {
//    val maybeTransaction = synchronized {
//      position(txn) match {
//        case Found(position) =>
//          notifyUnchanged(position).bounce
//        case _ =>
//          throw new IllegalStateException("Cannot process no-change notification - no Frame for " + txn)
//      }
//    }
//    maybeUserCompAndReevOut(maybeTransaction)
//  }
//
//  /**
//    * receive an external change notification within the given transaction. Executes all pipelined reevaluations
//    * of this node that become ready due to this notification. Notifications (change and no-change) for other nodes
//    * spawned by any of these reevaluations are queued in the [[Host.taskPool]] of [[host]] for concurrent processing.
//    * @param txn the transaction
//    */
//  def notifyChanged(txn: Transaction): Unit = synchronized {
//    val maybeTransaction = synchronized {
//      position(txn) match {
//        case Found(position) =>
//          _versions(position) match {
//            case frame: Frame if (frame.pending > 0) =>
//              frame.pending -= 1
//              if (frame.pending == 0) {
//                // frame.changed > 0 is implicit because this itself is a change notification
//                Some(frame.txn)
//              } else {
//                frame.changed += 1
//                frame.txn.branches(-1)
//                None
//              }
//            case version =>
//              throw new IllegalStateException("Cannot process change notification - not a Frame: " + version)
//          }
//        case _ =>
//          throw new IllegalStateException("Cannot process change notification - no Frame for " + txn)
//      }
//    }
//    maybeUserCompAndReevOut(maybeTransaction)
//  }

  // =================== READ OPERATIONS ====================

  private def ensureReadVersion(txn: Transaction): Int = {
    val position = find(txn)
    if(position < 0) {
      _versions.insert(-position, new Version(txn, _versions(-position - 1).out, pending = 0, changed = 0, None))
      -position
    } else {
      position
    }
  }

  def before(txn: Transaction): V = synchronized {
    @tailrec
    def before0(txn: Transaction): V = {
      val thisPosition = ensureReadVersion(txn)
      val readFrom = prevWrite(thisPosition)
      if(readFrom.isWritten) {
        readFrom.read()
      } else {
        blockUntilNoLongerFrame(readFrom.txn)
        before0(txn)
      }
    }
    before0(txn)
  }

  def now(txn: Transaction): V = synchronized {
    val position = ensureReadVersion(txn)
    if(_versions(position).isWritten) {
      _versions(position).read()
    } else {
      before(txn)
    }
  }

  def after(txn: Transaction): V = synchronized {
    @tailrec
    def after0(txn: Transaction): V = {
      val thisPosition = ensureReadVersion(txn)
      val thisVersion = _versions(thisPosition)
      if(thisVersion.isWritten) {
        thisVersion.read()
      } else if (thisVersion.isWrite) {
        blockUntilNoLongerFrame(thisVersion.txn)
        after0(txn)
      } else {
        val prevReev = prevWrite(thisPosition)
        if (prevReev.isWritten) {
          prevReev.read()
        } else {
          blockUntilNoLongerFrame(prevReev.txn)
          after0(txn)
        }
      }
    }
    after0(txn)
  }

  private def regReadPred(position: Int) = {
    prevWrite(position).read()
  }

  def regRead(txn: Transaction): V = synchronized {
    val position = findExecuted(txn)
    if(position > 0) {
      val thisVersion = _versions(position)
      if (thisVersion.isWrite) {
        thisVersion.read()
      } else {
        regReadPred(position)
      }
    } else {
      regReadPred(-position)
    }
  }

  // =================== DYNAMIC OPERATIONS ====================

  def discoverSuspend(txn: Transaction, add: O): V = {
    val (result, rewrite) = synchronized {
      val position = ensureReadVersion(txn)
      val result = after(txn)
      _versions(position).out += add
      val rewrite = retrofitDiscover(position, add)
      (result, rewrite)
    }
    if(rewrite.size > 0) add.retrofitDiscoverFrames(rewrite)
    result
  }

  private def retrofitDiscover(position: Int, add: O): RewriteFramesBuffer = {
    val rewrite = new RewriteFramesBuffer(_versions.size - position)
    for(pos <- position + 1 until _versions.size) {
      val version = _versions(pos)
      version.out += add
      if(version.isWrite) {
        if(version.isWritten) {
          rewrite.addWritten(version.txn)
        } else {
          rewrite.addFramed(version.txn)
        }
      }
    }
    rewrite
  }

  def retrofitDiscoverFrames(rewrite: RewriteFramesBuffer): Unit = synchronized {
    @tailrec
    def retrofitDiscoverFrames0(rewrite: RewriteFramesBuffer, idx: Int, minPos: Int): Unit = {
      if(idx < rewrite.size) {
        val txn = rewrite(idx)
        val position = findOrPidgeonHole(txn, minPos, minPos, _versions.size)
        if (position >= 0) {
          if (rewrite.isWritten(idx)) {
            _versions(position).changed += 1
          } else {
            _versions(position).pending += 1
          }
        } else {
          val frame = if (rewrite.isWritten(idx)) {
            new Version(txn, _versions(-position - 1).out, pending = 0, changed = 1, None)
          } else {
            new Version(txn, _versions(-position - 1).out, pending = 1, changed = 0, None)
          }
          insert(-position, frame)
        }
        retrofitDiscoverFrames0(rewrite, idx + 1, position + 1)
      }
    }
    retrofitDiscoverFrames0(rewrite, 0, firstFrame + 1)
  }

  def drop(txn: Transaction, remove: O): Unit = synchronized{
    val rewrite = synchronized {
      val position = ensureReadVersion(txn)
      val result = after(txn) // TODO technically unnecessary, think about if removing it violates anything.
      _versions(position).out -= remove
      retrofitDrop(position, remove)
    }
    if(rewrite.size > 0) remove.retrofitDropFrames(rewrite)
  }

  private def retrofitDrop(position: Int, remove: O): RewriteFramesBuffer = {
    val rewrite = new RewriteFramesBuffer(_versions.size - position)
    for(pos <- position + 1 until _versions.size) {
      val version = _versions(pos)
      version.out -= remove
      if(version.isWrite) {
        if(version.isWritten) {
          rewrite.addWritten(version.txn)
        } else {
          rewrite.addFramed(version.txn)
        }
      }
    }
    rewrite
  }

  def retrofitDropFrames(rewrite: RewriteFramesBuffer): Unit = synchronized {
    @tailrec
    def retrofitDropFrames0(rewrite: RewriteFramesBuffer, idx: Int, minPos: Int): Unit = {
      if(idx < rewrite.size) {
        val txn = rewrite(idx)
        val position = findOrPidgeonHole(txn, minPos, minPos, _versions.size)
        if (position >= 0) {
          if (rewrite.isWritten(idx)) {
            _versions(position).changed -= 1
          } else {
            _versions(position).pending -= 1
          }
          if(_versions(position).isReadOrDynamic) frameRemoved(txn)
        } else {
          val frame = if (rewrite.isWritten(idx)) {
            new Version(txn, _versions(-position - 1).out, pending = 0, changed = -1, None)
          } else {
            new Version(txn, _versions(-position - 1).out, pending = -1, changed = 0, None)
          }
          insert(-position, frame)
        }
        retrofitDropFrames0(rewrite, idx + 1, position + 1)
      }
    }
    retrofitDropFrames0(rewrite, 0, firstFrame + 1)
  }
}
