package rescala.fullmv

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.LockSupport
import java.util.concurrent.{ConcurrentHashMap, ForkJoinTask}

import rescala.core.Initializer.InitValues
import rescala.core._
import rescala.fullmv.NotificationResultAction._
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation._
import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.SubsumableLockEntryPoint
import rescala.fullmv.tasks.{Notification, Reevaluation}

import scala.annotation.tailrec

trait FullMVTurn extends Initializer[FullMVStruct] with FullMVTurnProxy with SubsumableLockEntryPoint with Hosted[FullMVTurn] {
  override val host: FullMVEngine

  //========================================================Internal Management============================================================

  // ===== Turn State Manangement External API
// TODO draft for async turn phase transitions
//  val executingWaiters = AtomicReference[List[() => Unit]]
//  val completionWaiters = AtomicReference[List[() => Unit]]
  val waiters = new ConcurrentHashMap[Thread, TurnPhase.Type]()
  def wakeWaitersAfterPhaseSwitch(newPhase: TurnPhase.Type): Unit = {
    val it = waiters.entrySet().iterator()
    while (it.hasNext) {
      val waiter = it.next()
      if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this phase switch unparking ${waiter.getKey.getName}.")
      if (waiter.getValue <= newPhase) LockSupport.unpark(waiter.getKey)
    }
  }

  override def accessTicket(): AccessTicket[FullMVStruct] = new AccessTicket[FullMVStruct](){
    override def access(reactive: ReSource[FullMVStruct]): reactive.Value = dynamicAfter(reactive)
  }

  def selfNode: TransactionSpanningTreeNode[FullMVTurn]
  // should be mirrored/buffered locally
  def phase: TurnPhase.Type // must implement a read barrier
  def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit
  def newBranchFromRemote(forState: TurnPhase.Type): Unit

  // ===== Ordering Search&Establishment External API
  // should be mirrored/buffered locally
  def isTransitivePredecessor(txn: FullMVTurn): Boolean

  //========================================================Remote Replication============================================================

  val phaseReplicators: AtomicReference[List[FullMVTurnPhaseReflectionProxy]] = new AtomicReference(Nil) // implicit set, write accesses are synchronized through CAS
  override def asyncAddPhaseReplicator(replicator: FullMVTurnPhaseReflectionProxy, knownPhase: TurnPhase.Type): Unit = {
    if(phase < TurnPhase.Completed) {
      val added = FullMVTurn.atomicAdd(phaseReplicators, replicator)
      assert(added || phase == TurnPhase.Completed, s"phase replicator addition should only return failure, if $this is completed")
      if(knownPhase < phase) replicator.asyncNewPhase(phase)
    } else {
      replicator.asyncNewPhase(TurnPhase.Completed)
    }
  }

  def clockedPredecessors: (TransactionSpanningTreeNode[FullMVTurn], Int)
  val predecessorReplicators: AtomicReference[List[FullMVTurnPredecessorReflectionProxy]] = new AtomicReference(Nil) // implicit set, write accesses are synchronized through CAS


  override def asyncAddPredecessorReplicator(replicator: FullMVTurnPredecessorReflectionProxy, startAt: TransactionSpanningTreeNode[FullMVTurn], clock: Int): Unit = {
    if(phase < TurnPhase.Completed) {
      val added = FullMVTurn.atomicAdd(predecessorReplicators, replicator)
      if(!added) {
        assert(phase == TurnPhase.Completed, s"phase replicator addition should only return failure, if $this is completed")
      } else {
        ensurePredecessorReplication(startAt, clock)
        val (knownPreds, knownClock) = clockedPredecessors
        if(clock < knownClock) replicator.newPredecessors(knownPreds, knownClock)
      }
    }
  }

  final def ensurePredecessorReplication(clockedPredecessors: (TransactionSpanningTreeNode[FullMVTurn], Int)): Unit = ensurePredecessorReplication(clockedPredecessors._1, clockedPredecessors._2)
  def ensurePredecessorReplication(startAt: TransactionSpanningTreeNode[FullMVTurn], clock: Int): Unit

  //========================================================Scheduler Interface============================================================

  override def makeDerivedStructState[P](valuePersistency: InitValues[P], creationTicket: CreationTicket[FullMVStruct]): NonblockingSkipListVersionHistory[P, FullMVTurn, ReSource[FullMVStruct], Derived[FullMVStruct]] = {
    val state = new NonblockingSkipListVersionHistory[P, FullMVTurn, ReSource[FullMVStruct], Derived[FullMVStruct]](host.dummy, valuePersistency)
    state.incrementFrame(this)
    state
  }

  override protected def makeSourceStructState[P](valuePersistency: InitValues[P], creationTicket: CreationTicket[FullMVStruct]): NonblockingSkipListVersionHistory[P, FullMVTurn, ReSource[FullMVStruct], Derived[FullMVStruct]] = {
    val state = makeDerivedStructState(valuePersistency, creationTicket)
    val res = state.notify(this, changed = false)
    assert(res == true -> PureNotifyOnly(Set.empty))
    state
  }

  override def ignite(reactive: Derived[FullMVStruct], incoming: Set[ReSource[FullMVStruct]], ignitionRequiresReevaluation: Boolean): Unit = {
//    assert(Thread.currentThread() == userlandThread, s"$this ignition of $reactive on different thread ${Thread.currentThread().getName}")
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this igniting $reactive on $incoming")
    incoming.foreach { discover =>
      discover.state.dynamicAfter(this) // TODO should we get rid of this?
    val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(this, reactive)
      reactive.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1).foreach(_.activeBranchDifferential(TurnPhase.Executing, 1))
    }
    reactive.state.incomings = incoming
    // Execute this notification manually to be able to execute a resulting reevaluation immediately.
    // Subsequent reevaluations from retrofitting will be added to the global pool, but not awaited.
    // This matches the required behavior where the code that creates this reactive is expecting the initial
    // reevaluation (if one is required) to have been completed, but cannot access values from subsequent turns
    // and hence does not need to wait for those.
    activeBranchDifferential(TurnPhase.Executing, 1)
    val ignitionNotification = new Notification(this, reactive, changed = ignitionRequiresReevaluation)
    ignitionNotification.deliverNotification() match {
      case (true, DoNothing) =>
        if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this ignite $reactive spawned a branch.")
      case (false, DoNothing) =>
        if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this ignite $reactive did not spawn a branch or reevaluation.")
        activeBranchDifferential(TurnPhase.Executing, -1)
      case (retainBranch, ReevaluationReady) =>
        if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this ignite $reactive spawned reevaluation.")
        new Reevaluation(this, reactive).doReevaluation(retainBranch)
      case (true, NotifyAndReevaluationReadySuccessor(out, succTxn)) if out.isEmpty =>
        if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this ignite $reactive spawned reevaluation for successor $succTxn.")
        activeBranchDifferential(TurnPhase.Executing, -1)
        val succReev = new Reevaluation(succTxn, reactive)
        if(ForkJoinTask.inForkJoinPool()) {
          succReev.fork()
        } else {
          host.threadPool.submit(succReev)
        }
      case (true, PureNotifyOnly(out)) if out.isEmpty=>
        activeBranchDifferential(TurnPhase.Executing, -1)
      case (true, NotifyAndNonReadySuccessor(out, _)) if out.isEmpty=>
        activeBranchDifferential(TurnPhase.Executing, -1)
      case other =>
        throw new AssertionError(s"$this ignite $reactive: unexpected result: $other")
    }
  }

  def discover(node: ReSource[FullMVStruct], addOutgoing: Derived[FullMVStruct]): Unit = {
    val r@(successorWrittenVersions, maybeFollowFrame) = node.state.discover(this, addOutgoing)
//    assert((successorWrittenVersions ++ maybeFollowFrame).forall(retrofit => retrofit == this || retrofit.isTransitivePredecessor(this)), s"$this retrofitting contains predecessors: discover $node -> $addOutgoing retrofits $r from ${node.state}")
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] Reevaluation($this,$addOutgoing) discovering $node -> $addOutgoing re-queueing $successorWrittenVersions and re-framing $maybeFollowFrame")
    val newBranches = addOutgoing.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
    newBranches.foreach(_.activeBranchDifferential(TurnPhase.Executing, 1))
  }

  def drop(node: ReSource[FullMVStruct], removeOutgoing: Derived[FullMVStruct]): Unit = {
    val r@(successorWrittenVersions, maybeFollowFrame) = node.state.drop(this, removeOutgoing)
//    assert((successorWrittenVersions ++ maybeFollowFrame).forall(retrofit => retrofit == this || retrofit.isTransitivePredecessor(this)), s"$this retrofitting contains predecessors: drop $node -> $removeOutgoing retrofits $r from ${node.state}")
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] Reevaluation($this,$removeOutgoing) dropping $node -> $removeOutgoing de-queueing $successorWrittenVersions and de-framing $maybeFollowFrame")
    val deletedBranches = removeOutgoing.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, -1)
    deletedBranches.foreach(_.activeBranchDifferential(TurnPhase.Executing, -1))
  }

  private[rescala] def writeIndeps(node: Derived[FullMVStruct], indepsAfter: Set[ReSource[FullMVStruct]]): Unit = node.state.incomings = indepsAfter

  private[rescala] def staticBefore(reactive: ReSource[FullMVStruct]) = reactive.state.staticBefore(this)
  private[rescala] def staticAfter(reactive: ReSource[FullMVStruct]) = reactive.state.staticAfter(this)
  private[rescala] def dynamicBefore(reactive: ReSource[FullMVStruct]) = reactive.state.dynamicBefore(this)
  private[rescala] def dynamicAfter(reactive: ReSource[FullMVStruct]) = reactive.state.dynamicAfter(this)

  def observe(f: () => Unit): Unit = f()
}

object FullMVTurn {
  def atomicAdd[T](list: AtomicReference[List[T]], element: T): Boolean = {
    @tailrec def tryAdd(): Boolean = {
      val before = list.get()
      if (before != null){
        if (!list.compareAndSet(before, element :: before)){
          tryAdd()
        } else {
          true
        }
      } else {
        false
      }
    }
    tryAdd()
  }

}
