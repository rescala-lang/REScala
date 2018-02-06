package rescala.fullmv

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.LockSupport
import java.util.concurrent.{ConcurrentHashMap, ForkJoinTask}

import rescala.core.Initializer.InitValues
import rescala.core._
import rescala.fullmv.NotificationResultAction._
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation._
import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.mirrors._
import rescala.fullmv.sgt.synchronization.SubsumableLockEntryPoint
import rescala.fullmv.tasks.{Notification, Reevaluation}

import scala.annotation.tailrec
import scala.concurrent.Future

trait FullMVTurn extends Initializer[FullMVStruct] with FullMVTurnProxy with SubsumableLockEntryPoint with Hosted[FullMVTurn] {
  override val host: FullMVEngine

  //========================================================Internal Management============================================================

  // ===== Turn State Manangement External API
  val waiters = new ConcurrentHashMap[Thread, TurnPhase.Type]()
  def wakeWaitersAfterPhaseSwitch(newPhase: Type): Unit = {
    val it = waiters.entrySet().iterator()
    while (it.hasNext) {
      val waiter = it.next()
      if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this phase switch unparking ${waiter.getKey.getName}.")
      if (waiter.getValue <= newPhase) LockSupport.unpark(waiter.getKey)
    }
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
  override def asyncAddPhaseReplicator(replicator: FullMVTurnPhaseReflectionProxy): Unit = {
    if(phase < TurnPhase.Completed) {
      val added = FullMVTurn.atomicAdd(phaseReplicators, replicator)
      assert(added || phase == TurnPhase.Completed, s"phase replicator addition should only return failure, if $this is completed")
      replicator.asyncNewPhase(phase)
    } else {
      replicator.asyncNewPhase(TurnPhase.Completed)
    }
  }

  val predecessorReplicators: AtomicReference[List[FullMVTurnPredecessorReflectionProxy]] = new AtomicReference(Nil) // implicit set, write accesses are synchronized through CAS
  override def addPredecessorReplicator(replicator: FullMVTurnPredecessorReflectionProxy): Future[TransactionSpanningTreeNode[FullMVTurn]] = {
    if(phase < TurnPhase.Completed) {
      val added = FullMVTurn.atomicAdd(predecessorReplicators, replicator)
      if(!added) {
        assert(phase == TurnPhase.Completed, s"phase replicator addition should only return failure, if $this is completed")
        Future.successful(CaseClassTransactionSpanningTreeNode(this, Array.empty))
      } else {
        val preds = selfNode
        if(preds == null)  {
          assert(phase == TurnPhase.Completed, s"predecessor tree root should have been initialized for this call to be possible, and should not have been deallocated yet as $this isn't completed")
          Future.successful(CaseClassTransactionSpanningTreeNode(this, Array.empty))
        } else {
          Future.successful(preds)
        }
      }
    } else {
      Future.successful(CaseClassTransactionSpanningTreeNode(this, Array.empty))
    }
  }

  def ensurePredecessorReplication(): Unit

  //========================================================Scheduler Interface============================================================

  override def makeDerivedStructState[P](valuePersistency: InitValues[P]): NodeVersionHistory[P, FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]] = {
    val state = new NodeVersionHistory[P, FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]](host.dummy, valuePersistency)
    state.incrementFrame(this)
    state
  }

  override protected def makeSourceStructState[P](valuePersistency: InitValues[P]): NodeVersionHistory[P, FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]] = {
    val state = makeDerivedStructState(valuePersistency)
    val res = state.notify(this, changed = false)
    assert(res == NoSuccessor(Set.empty))
    state
  }

  override def ignite(reactive: Reactive[FullMVStruct], incoming: Set[ReSource[FullMVStruct]], ignitionRequiresReevaluation: Boolean): Unit = {
//    assert(Thread.currentThread() == userlandThread, s"$this ignition of $reactive on different thread ${Thread.currentThread().getName}")
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this igniting $reactive on $incoming")
    incoming.foreach { discover =>
      discover.state.dynamicAfter(this) // TODO should we get rid of this?
    val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(this, reactive)
      reactive.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
    }
    reactive.state.incomings = incoming
    // Execute this notification manually to be able to execute a resulting reevaluation immediately.
    // Subsequent reevaluations from retrofitting will be added to the global pool, but not awaited.
    // This matches the required behavior where the code that creates this reactive is expecting the initial
    // reevaluation (if one is required) to have been completed, but cannot access values from subsequent turns
    // and hence does not need to wait for those.
    val ignitionNotification = Notification(this, reactive, changed = ignitionRequiresReevaluation)
    ignitionNotification.deliverNotification() match {
      case NotGlitchFreeReady =>
        if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this ignite $reactive did not spawn reevaluation.")
      // ignore
      case GlitchFreeReady =>
        if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this ignite $reactive spawned reevaluation.")
        activeBranchDifferential(TurnPhase.Executing, 1)
        Reevaluation(this, reactive).compute()
      case NextReevaluation(out, succTxn) if out.isEmpty =>
        if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this ignite $reactive spawned reevaluation for successor $succTxn.")
        succTxn.activeBranchDifferential(TurnPhase.Executing, 1)
        val succReev = Reevaluation(succTxn, reactive)
        if(ForkJoinTask.inForkJoinPool()) {
          succReev.fork()
        } else {
          host.threadPool.submit(succReev)
        }
      case otherOut: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]] if otherOut.out.isEmpty =>
      // ignore
      case other =>
        throw new AssertionError(s"$this ignite $reactive: unexpected result: $other")
    }
  }


  private[rescala] def discover(node: ReSource[FullMVStruct], addOutgoing: Reactive[FullMVStruct]): Unit = {
    val r@(successorWrittenVersions, maybeFollowFrame) = node.state.discover(this, addOutgoing)
    assert((successorWrittenVersions ++ maybeFollowFrame).forall(retrofit => retrofit == this || retrofit.isTransitivePredecessor(this)), s"$this retrofitting contains predecessors: discover $node -> $addOutgoing retrofits $r from ${node.state}")
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] Reevaluation($this,$addOutgoing) discovering $node -> $addOutgoing re-queueing $successorWrittenVersions and re-framing $maybeFollowFrame")
    addOutgoing.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
  }

  private[rescala] def drop(node: ReSource[FullMVStruct], removeOutgoing: Reactive[FullMVStruct]): Unit = {
    val r@(successorWrittenVersions, maybeFollowFrame) = node.state.drop(this, removeOutgoing)
    assert((successorWrittenVersions ++ maybeFollowFrame).forall(retrofit => retrofit == this || retrofit.isTransitivePredecessor(this)), s"$this retrofitting contains predecessors: drop $node -> $removeOutgoing retrofits $r from ${node.state}")
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] Reevaluation($this,$removeOutgoing) dropping $node -> $removeOutgoing de-queueing $successorWrittenVersions and de-framing $maybeFollowFrame")
    removeOutgoing.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, -1)
  }

  private[rescala] def writeIndeps(node: Reactive[FullMVStruct], indepsAfter: Set[ReSource[FullMVStruct]]): Unit = node.state.incomings = indepsAfter

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
