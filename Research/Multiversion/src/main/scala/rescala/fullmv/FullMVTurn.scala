package rescala.fullmv

import java.util.concurrent.ForkJoinTask

import rescala.core.Reactive
import rescala.core.{Reactive, ReactiV, TurnImpl, ValuePersistency}
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation.{NextReevaluation, NoSuccessor}
import rescala.fullmv.NotificationResultAction.{GlitchFreeReady, NotificationOutAndSuccessorOperation}
import rescala.fullmv.mirrors.{FullMVTurnProxy, FullMVTurnReflectionProxy, Host, Hosted}
import rescala.fullmv.tasks.{Notification, Reevaluation}

import scala.concurrent.duration.Duration

abstract class FullMVTurn(val timeout: Duration) extends TurnImpl[FullMVStruct] with FullMVTurnProxy with Hosted {
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

  override def makeStructState[P](valuePersistency: ValuePersistency[P]): NodeVersionHistory[P, FullMVTurn, Reactive[FullMVStruct], Reactive[FullMVStruct]] = {
    val state = new NodeVersionHistory[P, FullMVTurn, Reactive[FullMVStruct], Reactive[FullMVStruct]](host.dummy, valuePersistency, timeout)
    state.incrementFrame(this)
    state
  }
  override def ignite(reactive: Reactive[FullMVStruct], incoming: Set[Reactive[FullMVStruct]], ignitionRequiresReevaluation: Boolean): Unit = {
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this igniting $reactive on $incoming")
    incoming.foreach { discover =>
      discover.state.dynamicAfter(this) // TODO should we get rid of this?
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
          host.threadPool.submit(followReev)
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

  override private[rescala] def writeIndeps(node: Reactive[FullMVStruct], indepsAfter: Set[Reactive[FullMVStruct]]): Unit = node.state.incomings = indepsAfter

  override private[rescala] def staticBefore[P](reactive: ReactiV[P, FullMVStruct]) = reactive.state.staticBefore(this)
  override private[rescala] def staticAfter[P](reactive: ReactiV[P, FullMVStruct]) = reactive.state.staticAfter(this)
  override private[rescala] def dynamicBefore[P](reactive: ReactiV[P, FullMVStruct]) = reactive.state.dynamicBefore(this)
  override private[rescala] def dynamicAfter[P](reactive: ReactiV[P, FullMVStruct]) = reactive.state.dynamicAfter(this)

  override def observe(f: () => Unit): Unit = f()
}
