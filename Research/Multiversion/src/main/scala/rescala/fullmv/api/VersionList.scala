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

class Version[V](val txn: Transaction, var out: Set[O], var pending: Int, var changed: Int, var value: Option[V]) {
  def isWritten: Boolean = value.isDefined
  def isFrame: Boolean = pending > 0 || (changed > 0 && !isWritten)
  def isWrite: Boolean = pending > 0 || changed > 0
  def isReadOrDynamic: Boolean = !isWrite
  def isReadyForReevaluation: Boolean = pending == 0 && changed > 0 && !isWritten
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

class SignalVersionList[V](val host: Host, init: Transaction, initialValue: V, val userComputation: (Transaction, V) => V) {
  // =================== STORAGE ====================

  var _versions = new ArrayBuffer[Version[V]](6)
  _versions += new Version[V](init, Set(), 0, 0, Some(initialValue))
  var latestValue: V = initialValue

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

  private def prevWrite(from: Int): Version[V] = {
    var position = from - 1
    while(position >= 0 && !_versions(position).isWrite) position -= 1
    if(position < 0) throw new IllegalArgumentException("Does not have a preceding Reevaluation: "+_versions(from))
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

  def incrementFrame(txn: Transaction): Unit = {
    assert(txn.phase == Preparing)
    synchronized { incrementFrame0(txn) }.processBranching(txn, host.taskPool)
  }

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

  def notify(txn: Transaction, changed: Boolean, maybeFollowFrame: Option[Transaction]): Unit = {
    val nextStep: NotificationResultAction[V] = synchronized {
      if(maybeFollowFrame.isDefined) {
        incrementFrame0(maybeFollowFrame.get)
        // ignore return value for branching out!
      }

      val position = findFrame(txn)
      val version = _versions(position)
      version.pending -= 1
      if(changed) version.changed += 1
      if(version.pending == 0) {
        if(position == firstFrame) {
          if(version.changed > 0) {
            GlitchFreeReady(version)
          } else {
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

  @tailrec
  private def progressReevaluation(version: Version[V]): Unit = {
    // do reevaluation
    val newValue = userComputation(version.txn, latestValue)
    val selfChanged = latestValue != newValue

    // update version and search for successor write
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

    notification.send(host.taskPool)

    // progress to next reevaluation
    if (notification.maybeNextReevaluation.isDefined) {
      progressReevaluation(notification.maybeNextReevaluation.get)
    }
  }

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
  private def ensureReadVersion(txn: Transaction): Int = {
    val position = find(txn)
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

  /**
    * entry point for reg-read(this, d) (i.e., read [[Version.value]] assuming edge this -> d exists)
    * @param txn the executing transaction
    * @return the corresponding [[Version.value]]
    */
  def regRead(txn: Transaction): V = synchronized {
    val position = findExecuted(txn)
    if(position >= 0) {
      val thisVersion = _versions(position)
      if (thisVersion.isWrite) {
        thisVersion.read()
      } else {
        prevWrite(position).read()
      }
    } else {
      prevWrite(-position).read()
    }
  }

  // =================== DYNAMIC OPERATIONS ====================

  /**
    * entry point for discover(this, add). May suspend.
    * @param txn the transaction executing
    * @param add the outgoing dependency to add
    * @return the appropriate [[Version.value]].
    */
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

  /**
    * rewrites all affected [[Version.out]] during discover(this, add), collecting transactions for reframing on add.
    * @param position the executing transaction's version's position
    * @param add the outgoing dpendency to add
    * @return the list of rewrites
    */
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

  /**
    * performs the reframings for a discover(n, this)
    * @param rewrite the reframings to perform
    */
  def retrofitDiscoverFrames(rewrite: RewriteFramesBuffer): Unit = synchronized {
    @tailrec
    def retrofitDiscoverFrames0(rewrite: RewriteFramesBuffer, idx: Int, minPos: Int): Unit = {
      if(idx < rewrite.size) {
        val txn = rewrite(idx)
        val position = findOrPidgeonHole(txn, minPos, minPos, _versions.size)
        val realPosition = if (position < 0) {
          createVersion(-position, txn)
          -position
        } else {
          position
        }
        if (rewrite.isWritten(idx)) {
          _versions(realPosition).changed += 1
        } else {
          _versions(realPosition).pending += 1
        }
        retrofitDiscoverFrames0(rewrite, idx + 1, realPosition + 1)
      }
    }
    retrofitDiscoverFrames0(rewrite, 0, firstFrame + 1)
  }

  /**
    * entry point for drop(this, remove); may suspend temporarily.
    * @param txn the executing transaction
    * @param remove the outgoing dependency to remove
    */
  def drop(txn: Transaction, remove: O): Unit = synchronized {
    val rewrite = synchronized {
      val position = ensureReadVersion(txn)
      val result = after(txn) // TODO technically unnecessary, think about if removing it violates anything.
      _versions(position).out -= remove
      retrofitDrop(position, remove)
    }
    if(rewrite.size > 0) remove.retrofitDropFrames(rewrite)
  }

  /**
    * rewrites all affected [[Version.out]] during drop(this, remove), collecting transactions for deframing on remove
    * @param position the executing transaction's version's position
    * @param remove the outgoing dependency to remove
    * @return the deframings to perform
    */
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

  /**
    * performs the deframings for a drop(n, this)
    * @param rewrite the deframings to perform
    */
  def retrofitDropFrames(rewrite: RewriteFramesBuffer): Unit = synchronized {
    @tailrec
    def retrofitDropFrames0(rewrite: RewriteFramesBuffer, idx: Int, minPos: Int): Unit = {
      if(idx < rewrite.size) {
        val txn = rewrite(idx)
        val position = findOrPidgeonHole(txn, minPos, minPos, _versions.size)
        val realPosition = if (position < 0) {
          createVersion(-position, txn)
          -position
        } else {
          position
        }
        if (rewrite.isWritten(idx)) {
          _versions(realPosition).changed -= 1
        } else {
          _versions(realPosition).pending -= 1
        }
        if(position > 0 && _versions(position).isReadOrDynamic) frameRemoved(txn)
        retrofitDropFrames0(rewrite, idx + 1, realPosition + 1)
      }
    }
    retrofitDropFrames0(rewrite, 0, firstFrame + 1)
  }
}
