package rescala.fullmv

import java.util.concurrent.ForkJoinTask

import rescala.engine.{InitializationImpl, ValuePersistency}
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation.{NextReevaluation, NoSuccessor}
import rescala.fullmv.NotificationResultAction.{GlitchFreeReady, NotificationOutAndSuccessorOperation}
import rescala.fullmv.TurnPhase.Type
import rescala.fullmv.sgt.reachability.DigraphNodeWithReachability
import rescala.fullmv.tasks.{Notification, Reevaluation}
import rescala.fullmv.sgt.synchronization.{SubsumableLock, SubsumableLockImpl}
import rescala.graph.{Pulsing, Reactive}

class FullMVTurn extends InitializationImpl[FullMVStruct] {
  object phaseLock
  @volatile var phase: TurnPhase.Type = TurnPhase.Initialized

  object branchLock
  // counts the sum of in-flight notifications, in-progress reevaluations.
  var activeBranches: Int = 0
  def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit = {
    assert(phase == forState, s"$this received branch differential for wrong state $phase")
    if(differential != 0) {
      branchLock.synchronized {
        activeBranches += differential
        if(activeBranches == 0) {
          branchLock.notifyAll()
        }
      }
    }
  }

  def awaitAndSwitchPhase(newPhase: TurnPhase.Type): Unit = {
    assert(newPhase > this.phase, s"$this cannot progress backwards to phase $phase.")
    while(this.phase != newPhase) {
      awaitBranchCountZero()
      val preds: Set[FullMVTurn] = awaitAllPredecessorsPhase(newPhase)
      tryAtomicCompareBranchesPlusPredsAndSwitchPhase(preds, newPhase)
    }
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this switched phase.")
  }

  private def awaitBranchCountZero() = {
    branchLock.synchronized {
      while (activeBranches > 0) {
        branchLock.wait()
      }
    }
  }

  val lock: SubsumableLock = new SubsumableLockImpl()
  val sgtNode: DigraphNodeWithReachability = new DigraphNodeWithReachability()
  var nonTransitivePredecessors: Set[FullMVTurn] = Set.empty

  def isTransitivePredecessor(candidate: FullMVTurn): Boolean = {
    sgtNode.isReachable(candidate.sgtNode)
  }

  def addPredecessor(predecessor: FullMVTurn): Unit = {
    assert(predecessor.lock.getLockedRoot.isDefined, s"establishing order $predecessor -> $this: predecessor not locked")
    assert(lock.getLockedRoot.isDefined, s"establishing order $predecessor -> $this: successor not locked")
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this new predecessor $predecessor.")
    // since we keep track of the past, predecessors in time are successors in the SSG.
    if(sgtNode.addSuccessor(predecessor.sgtNode)) {
      nonTransitivePredecessors += predecessor
    }
  }

  private def awaitAllPredecessorsPhase(atLeast: TurnPhase.Type): Set[FullMVTurn] = {
    val preds = nonTransitivePredecessors
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this awaiting phase $atLeast+ on predecessors $preds")
    preds.foreach { waitFor =>
      waitFor.awaitPhase(atLeast)
    }
    preds
  }

//  def switchPhase(phase: TurnPhase.Type): Unit = {
//    phaseLock.synchronized{
//      require(phase > this.phase, s"$this cannot progress backwards to phase $phase.")
//      this.phase = phase
//      if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this switched phase.")
//      phaseLock.notifyAll()
//    }
//  }

  private def tryAtomicCompareBranchesPlusPredsAndSwitchPhase(preds: Set[FullMVTurn], newPhase: Type): Unit = {
    phaseLock.synchronized {
      if (activeBranches == 0 && nonTransitivePredecessors == preds) {
        this.phase = newPhase
        if(newPhase == TurnPhase.Completed) sgtNode.discard()
        if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this switched phase.")
        phaseLock.notifyAll()
      }
    }
  }


  private def awaitPhase(atLeast: TurnPhase.Type): Unit = phaseLock.synchronized {
    while(phase < atLeast) {
      phaseLock.wait()
    }
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


  override private[rescala] def staticBefore[P](reactive: Pulsing[P, FullMVStruct]) = reactive.state.staticBefore(this)
  override private[rescala] def staticAfter[P](reactive: Pulsing[P, FullMVStruct]) = reactive.state.staticAfter(this)
  override private[rescala] def dynamicBefore[P](reactive: Pulsing[P, FullMVStruct]) = reactive.state.dynamicBefore(this)
  override private[rescala] def dynamicAfter[P](reactive: Pulsing[P, FullMVStruct]) = reactive.state.dynamicAfter(this)

  override def observe(f: () => Unit): Unit = f()

  override def toString: String = synchronized {
    "FullMVTurn(" + System.identityHashCode(this) + ", " + (phase match {
      case 0 => "Initialized"
      case 1 => "Framing("+activeBranches+")"
      case 2 => "Executing("+activeBranches+")"
      case 3 => "WrapUp"
      case 4 => "Completed"
    })+ ")"
  }
}
