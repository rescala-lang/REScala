package rescala.fullmv

import java.util.concurrent.ForkJoinTask
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.LockSupport

import rescala.core.{Reactive, ReadableReactive, TurnImpl, ValuePersistency}
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation.{NextReevaluation, NoSuccessor}
import rescala.fullmv.NotificationResultAction.{GlitchFreeReady, NotificationOutAndSuccessorOperation}
import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.tasks.{Notification, Reevaluation}
import rescala.fullmv.sgt.synchronization.{SubsumableLock, SubsumableLockImpl}

import scala.collection.mutable.ArrayBuffer

class FullMVTurn(val userlandThread: Thread) extends TurnImpl[FullMVStruct] {
  object phaseLock
  @volatile var phase: TurnPhase.Type = TurnPhase.Initialized
  val lock: SubsumableLock = new SubsumableLockImpl()
  val successorsIncludingSelf = ArrayBuffer[FullMVTurn](this)
  val selfNode = new TransactionSpanningTreeNode(this)
  @volatile var predecessorSpanningTreeNodes = Map(this -> selfNode)
  // counts the sum of in-flight notifications, in-progress reevaluations.
  var activeBranches = new AtomicInteger(0)

  // TODO must be remote callable
  // TODO should use local buffering?
  def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit = {
    assert(phase == forState, s"$this received branch differential for wrong state $phase")
    if(differential != 0) {
      val remaining = activeBranches.addAndGet(differential)
      if(remaining == 0) {
        LockSupport.unpark(userlandThread)
      }
    }
  }

  def awaitAndSwitchPhase(newPhase: TurnPhase.Type): Unit = {
    assert(newPhase > this.phase, s"$this cannot progress backwards to phase $phase.")
    while(this.phase != newPhase) {
      awaitBranchCountZero()
      val compare = awaitAllPredecessorsPhase(newPhase)
      tryAtomicCompareBranchesPlusPredsAndSwitchPhase(compare, newPhase)
    }
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this switched phase.")
  }

  private def tryAtomicCompareBranchesPlusPredsAndSwitchPhase(compare: Map[FullMVTurn, TransactionSpanningTreeNode[FullMVTurn]], newPhase: Type): Unit = {
    phaseLock.synchronized {
      if (activeBranches.get == 0 && (predecessorSpanningTreeNodes eq compare)) {
        this.phase = newPhase
        if(newPhase == TurnPhase.Completed) {
          predecessorSpanningTreeNodes = Map.empty
          selfNode.children.clear()
        }
        if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this switched phase.")
        phaseLock.notifyAll()
      }
    }
  }

  // TODO should be resolved by local mirroring
  private def awaitPhase(atLeast: TurnPhase.Type): Unit = phaseLock.synchronized {
    while(phase < atLeast) {
      phaseLock.wait()
    }
  }

  private def awaitBranchCountZero() = {
    while (activeBranches.get > 0) {
      LockSupport.park(this)
    }
  }

  def isTransitivePredecessor(txn: FullMVTurn): Boolean = {
    predecessorSpanningTreeNodes.contains(txn)
  }

  // TODO must be remote callable
  def addPredecessor(predecessor: FullMVTurn): Unit = {
    assert(predecessor.lock.getLockedRoot.isDefined, s"establishing order $predecessor -> $this: predecessor not locked")
    assert(lock.getLockedRoot.isDefined, s"establishing order $predecessor -> $this: successor not locked")
    assert(!isTransitivePredecessor(predecessor), s"attempted to establish already existing predecessor relation $predecessor -> $this")
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this new predecessor $predecessor.")
    for (successorOrSelf <- successorsIncludingSelf) {
      // TODO possible remote call
      successorOrSelf.maybeNewReachableSubtree(this, predecessor.selfNode)
    }
  }

  private def copySubTreeRootAndAssessChildren(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]) = {
    val newTransitivePredecessor = spanningSubTreeRoot.txn
    newTransitivePredecessor.successorsIncludingSelf += this
    val copiedSpanningTreeNode = new TransactionSpanningTreeNode(newTransitivePredecessor)
    predecessorSpanningTreeNodes += newTransitivePredecessor -> copiedSpanningTreeNode
    predecessorSpanningTreeNodes(attachBelow).children.add(copiedSpanningTreeNode)

    for (child <- scala.collection.JavaConverters.collectionAsScalaIterable(spanningSubTreeRoot.children)) {
      maybeNewReachableSubtree(newTransitivePredecessor, child)
    }
  }

  // TODO remote callable
  // TODO should collect newly reachable nodes, and top level call should broadcast them to enable local mirroring; requires pointer to FullMVTurn or class merge
  private def maybeNewReachableSubtree(attachBelow: FullMVTurn, spanningSubTreeRoot: TransactionSpanningTreeNode[FullMVTurn]): Unit = {
    if (!isTransitivePredecessor(spanningSubTreeRoot.txn)) copySubTreeRootAndAssessChildren(attachBelow, spanningSubTreeRoot)
  }

  private def awaitAllPredecessorsPhase(atLeast: TurnPhase.Type) = {
    val preds = predecessorSpanningTreeNodes
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this awaiting phase $atLeast+ on predecessors $preds")
    preds.keySet.foreach { waitFor =>
      if(waitFor != this) waitFor.awaitPhase(atLeast)
    }
    preds
  }

  override protected def makeStructState[P](valuePersistency: ValuePersistency[P]): NodeVersionHistory[P, FullMVTurn, Reactive[FullMVStruct]] = {
    val state = new NodeVersionHistory[P, FullMVTurn, Reactive[FullMVStruct]](FullMVEngine.sgt, FullMVEngine.CREATE_PRETURN, valuePersistency)
    state.incrementFrame(this)
    state
  }
  override protected def ignite(reactive: Reactive[FullMVStruct], incoming: Set[Reactive[FullMVStruct]], ignitionRequiresReevaluation: Boolean): Unit = {
    incoming.foreach { discover =>
      val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(this, reactive)
      reactive.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
    }
    reactive.state.incomings = incoming
    val ignitionNotification = Notification(this, reactive, changed = ignitionRequiresReevaluation)
    // Execute this notification manually to be able to execute a resulting reevaluation immediately.
    // Subsequent reevaluations from retrofitting will be added to the global pool, but not awaited.
    // This matches the required behavior where the code that creates this reactive is expecting the initial
    // reevaluation (if one is required) to have been completed, but cannot access values from subsequent turns
    // and hence does not need to wait for those.
    val notificationResult = ignitionNotification.deliverNotification()
    val followNotification = notificationResult match {
      case GlitchFreeReady =>
        val (notification, _) = Reevaluation.doReevaluation(this, reactive)
        notification
      case outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]] =>
        outAndSucc
      case _ =>
        NoSuccessor(Set.empty[Reactive[FullMVStruct]])
    }
    followNotification match {
      case NextReevaluation(out, succTxn) =>
        assert(out.isEmpty, "newly created reactive should not be able to have outgoing dependencies")
        val followReev = new Reevaluation(succTxn, reactive)
        if (ForkJoinTask.inForkJoinPool()) {
          // this should be the case if reactive is created during another reevaluation
          followReev.fork()
        } else {
          // this should be the case if reactive is created during admission or wrap-up phase
          FullMVEngine.threadPool.submit(followReev)
        }
      case outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]] =>
        assert(outAndSucc.out.isEmpty, "newly created reactive should not be able to have outgoing dependencies")
    }
  }


  override private[rescala] def discover(node: Reactive[FullMVStruct], addOutgoing: Reactive[FullMVStruct]): Unit = {
    val (successorWrittenVersions, maybeFollowFrame) = node.state.discover(this, addOutgoing)
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] Reevaluation($this,$node) discovering $node -> $addOutgoing re-queueing $successorWrittenVersions and re-framing $maybeFollowFrame")
    addOutgoing.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
  }

  override private[rescala] def drop(node: Reactive[FullMVStruct], removeOutgoing: Reactive[FullMVStruct]): Unit = {
    val (successorWrittenVersions, maybeFollowFrame) = node.state.drop(this, removeOutgoing)
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] Reevaluation($this,$node) dropping $node -> $removeOutgoing de-queueing $successorWrittenVersions and de-framing $maybeFollowFrame")
    removeOutgoing.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, -1)
  }

  override private[rescala] def writeIndeps(node: Reactive[FullMVStruct], indepsAfter: Set[Reactive[FullMVStruct]]) = node.state.incomings = indepsAfter

  override private[rescala] def staticBefore[P](reactive: ReadableReactive[P, FullMVStruct]) = reactive.state.staticBefore(this)
  override private[rescala] def staticAfter[P](reactive: ReadableReactive[P, FullMVStruct]) = reactive.state.staticAfter(this)
  override private[rescala] def dynamicBefore[P](reactive: ReadableReactive[P, FullMVStruct]) = reactive.state.dynamicBefore(this)
  override private[rescala] def dynamicAfter[P](reactive: ReadableReactive[P, FullMVStruct]) = reactive.state.dynamicAfter(this)

  override def observe(f: () => Unit): Unit = f()

  override def toString: String = synchronized {
    "FullMVTurn(" + System.identityHashCode(this) + ", " + (phase match {
      case 0 => "Initialized"
      case 1 => "Framing("+activeBranches.get+")"
      case 2 => "Executing("+activeBranches.get+")"
      case 3 => "WrapUp"
      case 4 => "Completed"
    })+ ")"
  }
}
