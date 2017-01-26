package rescala.fullmv.api

import rescala.fullmv.rescala.fullmv.util.{Result, Recurse, Trampoline}

import scala.annotation.tailrec
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedMap, mutable}

class SignalVersionList[V](val host: Host, init: Transaction, initialValue: V, val userComputation: V => V) {
  type O = SignalVersionList[_]
  sealed abstract class Version (val txn: Transaction, var out: Set[O], val performedFraming: Boolean, val performedChangedPropagation: Boolean)
  final class ReadVersion(txn: Transaction, out: Set[O]) extends Version(txn, out, false, false)
  sealed abstract class Frame(txn: Transaction, out: Set[O]) extends Version(txn, out, true, false)
  final class Pending(txn: Transaction, out: Set[O], var pending: Int, var changed: Int) extends Frame(txn, out)
//  final class Ready(txn: Transaction, out: Set[O]) extends Frame(txn, out)
//  final class Active(txn: Transaction, out: Set[O]) extends Frame(txn, out)
  final class Written(txn: Transaction, out: Set[O], val value: V) extends Version(txn, out, true, true)

  var _versions = new ArrayBuffer[Version](6)
  _versions += new Written(init, Set(), initialValue)
  var latestValue: V = initialValue

  // =================== NAVIGATION ====================

  private def position(txn: Transaction): SearchResult = {
    @tailrec
    def ensureOrdered(txn: Transaction, attempt: InsertionPoint): SearchResult = {
      if(host.sgt.requireOrder(_versions(attempt.insertionPoint - 1).txn, txn) == FirstFirst) {
        attempt
      } else {
        import scala.collection.Searching._
        _versions.map(_.txn).search(txn)(host.sgt.fairSearchOrdering) match {
          case found @ Found(_) => found
          case attempt @ InsertionPoint(_) => ensureOrdered(txn, attempt)
          // TODO could optimize this case by not restarting with entire array bounds, but need to track fixedMin and min separately, following both above -1 cases.
        }
      }
    }
    ensureOrdered(txn, InsertionPoint(_versions.size))
  }

  private def prevReevPosition(from: Int): Int = {
    var prevReevPosition = from - 1
    while(prevReevPosition >= 0 && !_versions(prevReevPosition).performedFraming) prevReevPosition -= 1
    if(prevReevPosition < 0) throw new IllegalArgumentException("Does not have a preceding Reevaluation: "+_versions(from))
    prevReevPosition
  }

  private def nextReevPosition(from: Int): Option[Int] = {
    var nextReevPosition = from + 1
    while(nextReevPosition < _versions.size && !_versions(nextReevPosition).performedFraming) nextReevPosition += 1
    if(nextReevPosition < _versions.size) {
      Some(nextReevPosition)
    } else {
      None
    }
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
  }

  private def frameDropped(txn: Transaction): Unit = {
    // parameter included to allow parking-per-frame blocking/unblocking
    // but we implement coarse parking-per-node only, so we ignore it here.
    notifyAll()
    // Note that these should ideally be batched; one edge drop may result
    // in multiple frame drops each successive node, only notifying once at
    // the end would suffice for the coarse parking.
  }

  // =================== FRAMING ====================

  def incrementFrame(txn: Transaction): Unit = synchronized {
    position(txn) match {
      case Found(position) =>
        _versions(position) match {
          case frame: Pending =>
            frame.pending += 1
            txn.branches(-1)
          case version =>
            throw new IllegalStateException("Cannot incrementFrame: not a Frame: " + version)
        }
      case InsertionPoint(position) =>
        val out = _versions(position - 1).out
        val pending = new Pending(txn, out, pending = 1, changed = 0)
        if(_versions(prevReevPosition(position)).performedChangedPropagation) pending.pending += 1
        _versions.insert(position, pending)
        txn.branches(out.size - 1)
        out.foreach{node => host.taskPool.addFraming(txn, node)}
        out
    }
  }

  /*
   * =================== CHANGE / NOCHANGE / REEVALUATION ====================
   *
   * Note: This is a big pile of mutually recursive methods.
   * Upon any received change or no-change notification, this stack iterates through the version list
   * and executes all reevaluations that are not missing further notifications from other nodes.
   * The trampoline class is used to enable tail call optimization.
   * The control flow alternates between holding the monitor to count the notification and look up reevaluation input,
   * releasing the monitor to execute the reevaluation's user computation, and again holding the monitor to process
   * the reevaluation result and search for and update the pipeline-successive reevaluation before releasing it
   * again for executing the user computation again.
   * For a visualization of this control flow, consult the following diagram:
   * "doc/single node reevaluation pipeline mutual trampoline tail recursion.graphml"
   */

  /**
    * Updates active transaction branches and notify suspended reads after a completed reevaluation.
    * Tail-recursively traverses the version list onwards, looking for a pipeline-successive reevaluation.
    * @param position the version's position
    * @return Some(txn) to start a pipeline-successive reevaluation,
    *         or None if the pipeline-successor reevaluation is not ready or does not exists.
    */
  private def reevDone(position: Int): Trampoline[Option[Transaction]] = {
    val version = _versions(position)
    version.txn.branches(version.out.size - 1)
    frameRemoved(version.txn)
    nextReevPosition(position) match {
      case Some(next) => Recurse({ () => notifyUnchanged(next)})
      case None => Result(None)
    }
  }

  /**
    * Turns a frame into a read version after the frame was reevaluated as unchanged.
    * Tail-recursively traverses the version list onwards, looking for a pipeline-successive reevaluation.
    * @param position the version's position
    * @return Some(txn) to start a pipeline-successive reevaluation,
    *         or None if the next reevaluation is not ready or no next reevaluation exists.
    */
  private def reevUnchanged(position: Int): Trampoline[Option[Transaction]] = {
    val frame = _versions(position)
    _versions(position) = new ReadVersion(frame.txn, frame.out)
    frame.out.foreach{ succ => host.taskPool.addNoChangeNotification(frame.txn, succ) }
    reevDone(position)
  }

  /**
    * Turns a frame into a written version after the frame was reevaluated as changed.
    * Tail-recursively traverses the version list onwards, looking for a pipeline-successive reevaluation.
    * @param position the version's position
    * @param value the new value
    * @return Some(txn) to start a pipeline-successive reevaluation,
    *         or None if the pipeline-successor reevaluation is not ready or does not exists.
    */
  private def reevChanged(position: Int, value: V): Trampoline[Option[Transaction]] = {
    val frame = _versions(position)
    _versions(position) = new Written(frame.txn, frame.out, value)
    latestValue = value
    frame.out.foreach{ succ => host.taskPool.addChangeNotification(frame.txn, succ) }
    reevDone(position)
  }

  /**
    * Delivers a no-change to the frame at the given position, either from an external [[notifyUnchanged()]] message
    * or from a pipeline-predecessor reevaluation having completed.
    * Tail-recursively traverses the version list onwards, looking for a pipeline-successive reevaluation.
    * @param position the version's position
    * @return Some(txn) to start a pipeline-successive reevaluation,
    *         or None if the pipeline-successor reevaluation is not ready or does not exists.
    */
  private def notifyUnchanged(position: Int): Trampoline[Option[Transaction]] = {
    _versions(position) match {
      case frame: Pending if (frame.pending > 0) =>
        frame.pending -= 1
        if (frame.pending == 0) {
          if (frame.changed > 0) {
            Result(Some(frame.txn))
          } else {
            Recurse({ () => reevUnchanged(position)})
          }
        } else {
          frame.txn.branches(-1)
          Result(None)
        }
      case version =>
        throw new IllegalStateException("Cannot process no-change notification - not a Frame: " + version)
    }
  }

  /**
    * If the corresponding input is given, executes the reevaluation and tail-recursively all pipeline-successive
    * reevaluations that are ready. Note that the user computation runs without holding the node's monitor!
    * @param maybeTransaction Possibly a transaction to start a reevaluation.
    */
  @tailrec
  private def maybeUserCompAndReevOut(maybeTransaction: Option[Transaction]): Unit = {
    if(maybeTransaction.isDefined) {
      val txn = maybeTransaction.get
      val v_out = userComputation(latestValue)
      val maybeNextTransaction = if (latestValue == v_out) synchronized {
        position(txn) match {
          case Found(position) =>
            reevUnchanged(position).bounce
          case _ =>
            throw new AssertionError("Cannot reevOutUnchanged - Frame was deleted during userComputation for " + txn)
        }
      } else synchronized {
        position(txn) match {
          case Found(position) =>
            reevChanged(position, v_out).bounce
          case _ =>
            throw new AssertionError("Cannot reevOutChanged - Frame was deleted during userComputation for " + txn)
        }
      }
      maybeUserCompAndReevOut(maybeNextTransaction)
    }
  }

  /**
    * receive an external no-change notification within the given transaction. Executes all pipelined reevaluations
    * of this node that become ready due to this notification. Notifications (change and no-change) for other nodes
    * spawned by any of these reevaluations are queued in the [[Host.taskPool]] of [[host]] for concurrent processing.
    * @param txn the transaction
    */
  def notifyUnchanged(txn: Transaction): Unit = {
    val maybeTransaction = synchronized {
      position(txn) match {
        case Found(position) =>
          notifyUnchanged(position).bounce
        case _ =>
          throw new IllegalStateException("Cannot process no-change notification - no Frame for " + txn)
      }
    }
    maybeUserCompAndReevOut(maybeTransaction)
  }

  /**
    * receive an external change notification within the given transaction. Executes all pipelined reevaluations
    * of this node that become ready due to this notification. Notifications (change and no-change) for other nodes
    * spawned by any of these reevaluations are queued in the [[Host.taskPool]] of [[host]] for concurrent processing.
    * @param txn the transaction
    */
  def notifyChanged(txn: Transaction): Unit = synchronized {
    val maybeTransaction = synchronized {
      position(txn) match {
        case Found(position) =>
          _versions(position) match {
            case frame: Pending if (frame.pending > 0) =>
              frame.pending -= 1
              if (frame.pending == 0) {
                // frame.changed > 0 is implicit because this itself is a change notification
                Some(frame.txn)
              } else {
                frame.changed += 1
                frame.txn.branches(-1)
                None
              }
            case version =>
              throw new IllegalStateException("Cannot process change notification - not a Frame: " + version)
          }
        case _ =>
          throw new IllegalStateException("Cannot process change notification - no Frame for " + txn)
      }
    }
    maybeUserCompAndReevOut(maybeTransaction)
  }

  // =================== READ OPERATIONS ====================

  private def ensureReadVersion(txn: Transaction): Int = {
    position(txn) match {
      case Found(position) =>
        position
      case InsertionPoint(position) =>
        _versions.insert(position, new ReadVersion(txn, _versions(position - 1).out))
        position
    }
  }

  def before(txn: Transaction): V = synchronized {
    @tailrec
    def before0(txn: Transaction): V = {
      val thisPosition = ensureReadVersion(txn)
      val readPosition = prevReevPosition(thisPosition)
      _versions(readPosition) match {
        case written: Written =>
          written.value
        case frame: Frame =>
          blockUntilNoLongerFrame(frame.txn)
          before0(txn)
        case _ =>
          throw new AssertionError("This case should be impossible; prevReevPosition should only select Written or Frame positions; maybe synchronization bug!")
      }
    }
    before0(txn)
  }

  def now(txn: Transaction): V = synchronized {
    val position = ensureReadVersion(txn)
    _versions(position) match {
      case written: Written => written.value
      case _ =>
        before(txn)
    }
  }


  def after(txn: Transaction): V = synchronized {
    @tailrec
    def after0(txn: Transaction): V = {
      val thisPosition = ensureReadVersion(txn)
      _versions(thisPosition) match {
        case written: Written =>
          written.value
        case frame: Frame =>
          blockUntilNoLongerFrame(frame.txn)
          after0(txn)
        case _ =>
          val prevReev = prevReevPosition(thisPosition)
          _versions(prevReev) match {
            case written: Written =>
              written.value
            case frame: Frame =>
              blockUntilNoLongerFrame(frame.txn)
              after0(txn)
            case version =>
              throw new AssertionError("This case should be impossible, maybe synchronization bug! prevReevPosition should only select Written or Frame positions, but selected " + version)
          }
      }
    }
    after0(txn)
  }

  private def regReadPred(position: Int) = {
    _versions(prevReevPosition(position)) match {
      case written: Written =>
        written.value
      case version: Frame =>
        throw new IllegalStateException("Cannot regRead due to glitch-freedom violation: " + _versions(position).txn + " predecessor has incomplete " + version)
      case version =>
        throw new AssertionError("This case should be impossible, maybe synchronization bug! prevReevPosition should only select Written or Frame positions, but selected " + version)
    }
  }

  def regRead(txn: Transaction): V = synchronized {
    position(txn) match {
      case Found(position) =>
        _versions(position) match {
          case written: Written =>
            written.value
          case version: Frame =>
            throw new IllegalStateException("Cannot regRead due to glitch-freedom violation: transaction has own, but incomplete " + version)
          case version: ReadVersion =>
            regReadPred(position)
        }
      case InsertionPoint(position) =>
        regReadPred(position)
    }
  }

  // =================== DYNAMIC OPERATIONS ====================

  def discover(txn: Transaction, add: O): V = synchronized {
    val position = ensureReadVersion(txn)
    _versions(position).out += add
    for(pos <- (position + 1) until _versions.size) {
      val version = _versions(pos)
      version.out += add
      if(version.performedFraming) {
        if(version.performedChangedPropagation) {
          add.reincrementChanged(version.txn)
        } else {
          add.reincrementPending(version.txn)
        }
      }
    }
    after(txn)
  }

  def reincrementChanged(txn: Transaction): Unit = {
    position(txn) match {
      case Found(position) =>
        _versions(position) match {
          case pending: Pending =>
            pending.changed += 1
          case read: ReadVersion =>
            _versions(position) = new Pending(txn, read.out, pending = 1, changed = 1)
            read.out.foreach{node =>  node.reincrementPending(txn)}
          case version =>
            throw new IllegalStateException("Cannot reincrement on " + version)
        }
      case InsertionPoint(position) =>
        val out = _versions(position - 1).out
        _versions.insert(position, new Pending(txn, out, pending = 1, changed = 1))
        out.foreach{node =>  node.reincrementPending(txn)}
    }
  }

  def reincrementPending(txn: Transaction): Unit = {
    // TODO this bonus traversal should be unnecessary
    // the reevaluating transaction should be able to create these frames
    // on each node when completing its reevaluation (changed or unchanged)
    position(txn) match {
      case Found(position) =>
        _versions(position) match {
          case pending: Pending =>
            pending.pending += 1
          case read: ReadVersion =>
            _versions(position) = new Pending(txn, read.out, pending = 2, changed = 0)
            read.out.foreach { node => node.reincrementPending(txn) }
          case version =>
            throw new IllegalStateException("Cannot reincrement on " + version)
        }
      case InsertionPoint(position) =>
        val out = _versions(position - 1).out
        _versions.insert(position, new Pending(txn, out, pending = 2, changed = 0))
        out.foreach { node => node.reincrementPending(txn) }
    }
  }

  def drop(txn: Transaction, remove: O): Unit = synchronized{
    val position = ensureReadVersion(txn)
    _versions(position).out -= remove
    for(pos <- (position + 1) until _versions.size) {
      val version = _versions(pos)
      version.out -= remove
      if(version.performedFraming) {
        if(version.performedChangedPropagation) {
          remove.redecrementChanged(version.txn)
        } else {
          remove.redecrementPending(version.txn)
        }
      }
    }
  }

  def redecrementChanged(txn: Transaction): Unit = {
    position(txn) match {
      case Found(position) =>
        _versions(position) match {
          case pending: Pending =>
            pending.changed -= 1
          case version =>
            throw new IllegalStateException("Cannot redecrement on " + version)
        }
      case version =>
        throw new IllegalStateException("Cannot redecrement due to missing frame for txn" + txn)
    }
  }

  def redecrementPending(txn: Transaction): Unit = {
    position(txn) match {
      case Found(position) =>
        _versions(position) match {
          case pending: Pending if (pending.pending > 1) =>
            pending.pending -= 1
          case version =>
            throw new IllegalStateException("Cannot redecrement on " + version)
        }
      case version =>
        throw new IllegalStateException("Cannot redecrement due to missing frame for txn" + txn)
    }
  }
}
