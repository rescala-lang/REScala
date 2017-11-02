package rescala.fullmv

import java.util.concurrent.ForkJoinTask

import rescala.core._
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation.NoSuccessor
import rescala.fullmv.mirrors.{FullMVTurnProxy, FullMVTurnReflectionProxy, Host, Hosted}
import rescala.fullmv.tasks.{Notification, Reevaluation}

trait FullMVTurn extends TurnImpl[FullMVStruct] with FullMVTurnProxy with Hosted {
  override val host: FullMVEngine

  //========================================================Internal Management============================================================

  // ===== Turn State Manangement External API
  // should be mirrored/buffered locally
  def phase: TurnPhase.Type
  def awaitPhase(atLeast: TurnPhase.Type): Unit
  def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit
  def newBranchFromRemote(forState: TurnPhase.Type): Unit

  // ===== Ordering Search&Establishment External API
  // should be mirrored/buffered locally
  def isTransitivePredecessor(txn: FullMVTurn): Boolean

  // ===== Remote Replication Stuff
  // should be local-only, but needs to be available on remote mirrors too to support multi-hop communication.
  def addReplicator(replicator: FullMVTurnReflectionProxy): (TurnPhase.Type, Seq[Host.GUID])

  //========================================================Scheduler Interface============================================================

  override def makeDerivedStructState[P](valuePersistency: ValuePersistency[P]): NodeVersionHistory[P, FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]] = {
    val state = new NodeVersionHistory[P, FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]](host.dummy, valuePersistency)
    state.incrementFrame(this)
    state
  }

  override protected def makeSourceStructState[P](valuePersistency: ValuePersistency[P]): NodeVersionHistory[P, FullMVTurn, ReSource[FullMVStruct], Reactive[FullMVStruct]] = {
    val state = makeDerivedStructState(valuePersistency)
    val res = state.notify(this, changed = false)
    assert(res == NoSuccessor(Set.empty))
    state
  }

  override def ignite(reactive: Reactive[FullMVStruct], incoming: Set[ReSource[FullMVStruct]], ignitionRequiresReevaluation: Boolean): Unit = {
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
    activeBranchDifferential(TurnPhase.Executing, 1)
    val ignitionNotification = Notification(this, reactive, changed = ignitionRequiresReevaluation)
    val notificationResult = ignitionNotification.doCompute()
    if(notificationResult.nonEmpty) {
      assert(notificationResult.size == 1)
      assert(notificationResult.head == Reevaluation(this, reactive))
      val reevaluation = notificationResult.head.asInstanceOf[Reevaluation]
      val nextReev = reevaluation.doCompute()
      if(nextReev.nonEmpty) {
        assert(notificationResult.size == 1)
        assert(notificationResult.head.isInstanceOf[Reevaluation])
        val followReev = notificationResult.head.asInstanceOf[Reevaluation]
        if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this ignite $reactive reevaluated, delegating successor reevaluation for ${followReev.turn} to pool.")
        if (ForkJoinTask.inForkJoinPool()) {
          followReev.fork()
        } else {
          // this should be the case if reactive is created during admission or wrap-up phase
          host.threadPool.submit(followReev)
        }
      } else {
        if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this ignite $reactive reevaluated, no successor reevaluation.")
      }
    } else {
      if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this ignite $reactive did not spawn reevaluation.")
    }
  }


  override private[rescala] def discover(node: ReSource[FullMVStruct], addOutgoing: Reactive[FullMVStruct]): Unit = {
    val (successorWrittenVersions, maybeFollowFrame) = node.state.discover(this, addOutgoing)
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] Reevaluation($this,$addOutgoing) discovering $node -> $addOutgoing re-queueing $successorWrittenVersions and re-framing $maybeFollowFrame")
    addOutgoing.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
  }

  override private[rescala] def drop(node: ReSource[FullMVStruct], removeOutgoing: Reactive[FullMVStruct]): Unit = {
    val (successorWrittenVersions, maybeFollowFrame) = node.state.drop(this, removeOutgoing)
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] Reevaluation($this,$removeOutgoing) dropping $node -> $removeOutgoing de-queueing $successorWrittenVersions and de-framing $maybeFollowFrame")
    removeOutgoing.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, -1)
  }

  override private[rescala] def writeIndeps(node: Reactive[FullMVStruct], indepsAfter: Set[ReSource[FullMVStruct]]): Unit = node.state.incomings = indepsAfter

  override private[rescala] def staticBefore[P](reactive: ReSourciV[P, FullMVStruct]) = reactive.state.staticBefore(this)
  override private[rescala] def staticAfter[P](reactive: ReSourciV[P, FullMVStruct]) = reactive.state.staticAfter(this)
  override private[rescala] def dynamicBefore[P](reactive: ReSourciV[P, FullMVStruct]) = reactive.state.dynamicBefore(this)
  override private[rescala] def dynamicAfter[P](reactive: ReSourciV[P, FullMVStruct]) = reactive.state.dynamicAfter(this)

  override def observe(f: () => Unit): Unit = f()
}
