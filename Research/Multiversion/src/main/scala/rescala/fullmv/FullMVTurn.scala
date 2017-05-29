package rescala.fullmv

import java.util.concurrent.ForkJoinTask
import java.util.concurrent.atomic.AtomicInteger

import rescala.engine.{InitializationImpl, ValuePersistency}
import rescala.fullmv.NotificationResultAction.GlitchFreeReady
import rescala.fullmv.tasks.{Notification, Reevaluation}
import rescala.graph.{Pulsing, Reactive}

class FullMVTurn(val sgt: SerializationGraphTracking[FullMVTurn]) extends InitializationImpl[FullMVStruct] with TurnPhase {
  // counts the sum of in-flight notifications, in-progress reevaluations.
  @volatile var phase: TurnPhase.Type = TurnPhase.Initialized
  object stateParking
  var activeBranches: Int = 0
  val completedReevaluations = new AtomicInteger(0)

  def activeBranchDifferential(forState: TurnPhase.Type, differential: Int): Unit = synchronized {
    assert(phase == forState, s"$this received branch differential for wrong state $phase")
    activeBranches += differential
    if(activeBranches == 0) {
      notifyAll()
    }
  }

  def awaitBranches(): Int = synchronized {
    while(activeBranches > 0) {
      wait()
    }
    completedReevaluations.get
  }

  def beginPhase(state: TurnPhase.Type, initialActiveBranches: Int): Unit = synchronized {
    require(state > this.phase, s"$this cannot progress backwards to phase $state.")
    assert(this.activeBranches == 0, s"$this still has active branches and thus cannot start phase $state!")
    this.activeBranches = initialActiveBranches
    stateParking.synchronized{
      this.phase = state
      stateParking.notifyAll()
    }
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this switched phase.")
  }

  def awaitState(atLeast: TurnPhase.Type): Unit = stateParking.synchronized {
    while(phase < atLeast) {
      stateParking.wait()
    }
  }

  override protected def makeStructState[P](valuePersistency: ValuePersistency[P]): NodeVersionHistory[P, FullMVTurn, Reactive[FullMVStruct]] = {
    val state = new NodeVersionHistory[P, FullMVTurn, Reactive[FullMVStruct]](sgt, FullMVEngine.CREATE_PRETURN, valuePersistency)
    state.incrementFrame(this)
    state
  }
  override protected def ignite(reactive: Reactive[FullMVStruct], incoming: Set[Reactive[FullMVStruct]], ignitionRequiresReevaluation: Boolean): Unit = {
    activeBranchDifferential(TurnPhase.Executing, 1)
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
    if(notificationResult == GlitchFreeReady) {
      val reevaluation = Reevaluation(this, reactive)
      if (ForkJoinTask.inForkJoinPool()) {
        // this should be the case if reactive is created during another reevaluation
        reevaluation.invoke()
      } else {
        // this should be the case if reactive is created during admission or wrap-up phase
        FullMVEngine.threadPool.invoke(reevaluation)
      }
    } else {
      ignitionNotification.processNotificationResult(notificationResult)
    }
  }


  override private[rescala] def staticBefore[P](reactive: Pulsing[P, FullMVStruct]) = reactive.state.staticBefore(this)
  override private[rescala] def staticAfter[P](reactive: Pulsing[P, FullMVStruct]) = reactive.state.staticAfter(this)
  override private[rescala] def dynamicBefore[P](reactive: Pulsing[P, FullMVStruct]) = reactive.state.dynamicBefore(this)
  override private[rescala] def dynamicAfter[P](reactive: Pulsing[P, FullMVStruct]) = reactive.state.dynamicAfter(this)
  override private[rescala] def selfBefore[P](reactive: Pulsing[P, FullMVStruct]) = reactive.state.reevIn(this)

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
