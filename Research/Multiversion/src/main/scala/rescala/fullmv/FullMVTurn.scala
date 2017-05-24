package rescala.fullmv

import rescala.engine.{InitializationImpl, ValuePersistency}
import rescala.fullmv.FramingBranchResult._
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation._
import rescala.fullmv.NotificationResultAction._
import rescala.fullmv.wsdeque.WriteableQueue
import rescala.graph.{Pulsing, Reactive}
import rescala.graph.ReevaluationResult.{Dynamic, Static}

case class Framing(txn: FullMVTurn, node: Reactive[FullMVStruct]) extends Task[FullMVTurn, Reactive[FullMVStruct]] {
  override def apply(queue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = txn.incrementFrame(node, queue)
}
case class SupersedeFraming(txn: FullMVTurn, node: Reactive[FullMVStruct], supersede: FullMVTurn) extends Task[FullMVTurn, Reactive[FullMVStruct]] {
  override def apply(queue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = txn.incrementSupersedeFrame(node, supersede, queue)
}
case class Reevaluation(txn: FullMVTurn, node: Reactive[FullMVStruct]) extends Task[FullMVTurn, Reactive[FullMVStruct]] {
  override def apply(queue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = txn.reevaluate(node, queue)
}
case class Notification(txn: FullMVTurn, node: Reactive[FullMVStruct], changed: Boolean) extends Task[FullMVTurn, Reactive[FullMVStruct]] {
  override def apply(queue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = txn.notify(node, changed, queue)
}
case class NotificationWithFollowFrame(txn: FullMVTurn, node: Reactive[FullMVStruct], changed: Boolean, followFrame: FullMVTurn) extends Task[FullMVTurn, Reactive[FullMVStruct]] {
  override def apply(queue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = txn.notifyFollowFrame(node, changed, followFrame, queue)
}

class FullMVTurn(val sgt: SerializationGraphTracking[FullMVTurn]) extends InitializationImpl[FullMVStruct] {
  private lazy val preTurn = {
    val preTurn = new FullMVTurn(sgt)
    preTurn.beginPhase(State.Completed, -1)
    preTurn
  }

  /**
    * counts the sum of in-flight notifications, in-progress reevaluations.
    */
  @volatile var state: State.Type = State.Initialized
  object stateParking
  var activeBranches: Int = 0

  def activeBranchDifferential(forState: State.Type, differential: Int): Unit = synchronized {
    assert(state == forState, s"$this received branch differential for wrong state $state")
    activeBranches += differential
    if(activeBranches == 0) {
      notifyAll()
    }
  }

  def awaitBranches(): Unit = synchronized {
    while(activeBranches > 0) {
      wait()
    }
  }

  def beginPhase(state: State.Type, initialActiveBranches: Int): Unit = synchronized {
    require(state > this.state, s"$this cannot progress backwards to phase $state.")
    assert(this.activeBranches == 0, s"$this still has active branches and thus cannot start phase $state!")
    this.activeBranches = initialActiveBranches
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this switched phase.")
    stateParking.synchronized{
      this.state = state
      stateParking.notifyAll()
    }
  }

  def awaitState(atLeast: State.Type): Unit = stateParking.synchronized {
    while(state < atLeast) {
      stateParking.wait()
    }
  }

  def incrementFrame(node: Reactive[FullMVStruct], writeableQueue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = {
    assert(state == State.Framing, s"$this cannot increment frame (requires framing phase)")
    val framingBranchResult = node.state.incrementFrame(this)
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this framing $node resulting in $framingBranchResult")
    processFramingBranching(framingBranchResult, writeableQueue)
  }

  def incrementSupersedeFrame(node: Reactive[FullMVStruct], superseded: FullMVTurn, writeableQueue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = {
    assert(state == State.Framing, s"$this cannot increment frame (requires framing phase)")
    assert(superseded.state == State.Framing, s"$superseded cannot have frame superseded (requires framing phase)")
    val framingBranchResult = node.state.incrementSupersedeFrame(this, superseded)
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this framing $node superseding $superseded resulting in $framingBranchResult")
    processFramingBranching(framingBranchResult, writeableQueue)
  }

  private def processFramingBranching(result: FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]], writeableQueue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = {
    result match {
      case FramingBranchEnd =>
        activeBranchDifferential(State.Framing, -1)
        Traversable.empty
      case FramingBranchOut(out) =>
        if(out.size != 1) activeBranchDifferential(State.Framing, out.size - 1)
        for(succ <- out) writeableQueue.pushBottom(Framing(this, succ))
      case FramingBranchOutSuperseding(out, supersede) =>
        if(out.size != 1) activeBranchDifferential(State.Framing, out.size - 1)
        for(succ <- out) writeableQueue.pushBottom(SupersedeFraming(this, succ, supersede))
    }
  }

  def notify(node: Reactive[FullMVStruct], changed: Boolean, writeableQueue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = {
    assert(state == State.Executing, s"$this cannot receive notification (requires executing phase)")
    val notificationResultAction = node.state.notify(this, changed)
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this notified $node changed=$changed resulting in $notificationResultAction")
    processNotificationResultAction(node, notificationResultAction, writeableQueue)
  }

  def notifyFollowFrame(node: Reactive[FullMVStruct], changed: Boolean, followFrame: FullMVTurn, writeableQueue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = {
    assert(state == State.Executing, s"$this cannot receive notification (requires executing phase)")
    assert(followFrame.state >= State.Framing, s"$followFrame cannot receive follow frame (requires at least Framing phase)")
    assert(followFrame.state <= State.Executing, s"$followFrame cannot receive follow frame (requires at most Executing phase)")
    val notificationResultAction = node.state.notifyFollowFrame(this, changed, followFrame)
    if (FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this notified $node changed=$changed followFrame=$followFrame resulting in $notificationResultAction")
    processNotificationResultAction(node, notificationResultAction, writeableQueue)
  }

  def processNotificationResultAction(node: Reactive[FullMVStruct], notificationResultAction: NotificationResultAction[FullMVTurn, Reactive[FullMVStruct]], writeableQueue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = {
    notificationResultAction match {
      case GlitchFreeReadyButQueued =>
        activeBranchDifferential(State.Executing, -1)
      case ResolvedNonFirstFrameToUnchanged =>
        activeBranchDifferential(State.Executing, -1)
      case NotGlitchFreeReady =>
        activeBranchDifferential(State.Executing, -1)
      case GlitchFreeReady =>
        writeableQueue.pushBottom(Reevaluation(this, node))
      case outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]] =>
        processNotificationAndFollowOperation(node, changed = false, outAndSucc, writeableQueue)
    }
  }

  private def processNotificationAndFollowOperation(node: Reactive[FullMVStruct], changed: Boolean, outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]], writeableQueue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = {
    outAndSucc match {
      case NoSuccessor(out) =>
        if(out.size != 1) activeBranchDifferential(State.Executing, out.size - 1)
        for(succ <- out) writeableQueue.pushBottom(Notification(this, succ, changed))
      case FollowFraming(out, succTxn) =>
        if(out.size != 1) activeBranchDifferential(State.Executing, out.size - 1)
        for(succ <- out) writeableQueue.pushBottom(NotificationWithFollowFrame(this, succ, changed, succTxn))
      case NextReevaluation(out, succTxn) =>
        succTxn.activeBranchDifferential(State.Executing, 1)
        if(out.size != 1) activeBranchDifferential(State.Executing, out.size - 1)
        for(succ <- out) writeableQueue.pushBottom(NotificationWithFollowFrame(this, succ, changed, succTxn))
        writeableQueue.pushBottom(Reevaluation(succTxn, node))
    }
  }

  def reevaluate(node: Reactive[FullMVStruct], writeableQueue: WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]]): Unit = {
    assert(state == State.Executing, s"$this cannot reevaluate (requires executing phase")
    val result = FullMVEngine.withTurn(this){ node.reevaluate(this) }
    result match {
      case Static(isChange, value) =>
        val outAndSuccOp = node.state.reevOut(this, if(isChange) Some(value) else None)
        if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this reevaluated $node to $value, branching $outAndSuccOp")
        processNotificationAndFollowOperation(node, isChange, outAndSuccOp, writeableQueue)
      case res @ Dynamic(isChange, value, deps) =>
        val diff = res.depDiff(node.state.incomings)
        diff.removed.foreach{ drop =>
          val (successorWrittenVersions, maybeFollowFrame) = drop.state.drop(this, node)
          if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this dropping $drop -> $node un-queueing $successorWrittenVersions and un-framing $maybeFollowFrame")
          node.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, -1)
        }
        // TODO after adding in-turn parallelism, nodes may be more complete here than they were during actual reevaluation, leading to missed glitches
        val anyPendingDependency = diff.added.foldLeft(false) { (anyPendingDependency, discover) =>
          val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(this, node)
          if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this discovering $discover-> $node re-queueing $successorWrittenVersions and re-framing $maybeFollowFrame")
          node.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
                                  // this does not check for a frame by any preceding transaction but only for itself, because
                                  // the synchronization at the beginning of discover makes it so that no preceding frames exist;
                                  // should that be removed, we need to instead check, e.g.,
                                  // maybeFollowFrame.exists{txn => txn == this || sgt.getOrder(txn, this) == FirstFirst}
          anyPendingDependency || (maybeFollowFrame == Some(this))
        }
        // if(droppedOwnFrame) {
        //   if(rediscoveredOwnFrame) {
        //     System.err.println(s"[FullMV Warning] reevaluation of $node during $this re-routed all its incoming changed edges. Not sure if this should be legal.")
        //   } else {
        //     assert(!isChange, s"Impossible Reevaluation of $node during $this: Dropped all incoming changed edges, but still produced a change!")
        //     System.err.println(s"[FullMV Warning] reevaluation (unchanged) of $node during $this dropped all its incoming changed edges. This should probably be illegal, but dynamic events are implemented badly, causing this.")
        //   }
        // } else {
        //   assert(!rediscoveredOwnFrame, "either this is impossible or I am stupid.")
        // }
        node.state.incomings = deps
        if(anyPendingDependency) {
          activeBranchDifferential(State.Executing, -1)
        } else {
          val outAndSuccOp = node.state.reevOut(this, if (isChange) Some(value) else None)
          if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this reevaluated $node to $value, branching $outAndSuccOp")
          processNotificationAndFollowOperation(node, isChange, outAndSuccOp, writeableQueue)
        }
    }
  }

  override protected def makeStructState[P](valuePersistency: ValuePersistency[P]): NodeVersionHistory[P, FullMVTurn, Reactive[FullMVStruct]] = {
    val state = new NodeVersionHistory[P, FullMVTurn, Reactive[FullMVStruct]](sgt, preTurn, valuePersistency)
    state.incrementFrame(this)
    state
  }
  override protected def ignite(reactive: Reactive[FullMVStruct], incoming: Set[Reactive[FullMVStruct]], valuePersistency: ValuePersistency[_]): Unit = {
    activeBranchDifferential(State.Executing, 1)
    incoming.foreach { discover =>
      dynamicDependencyInteraction(discover)
      val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(this, reactive)
      reactive.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
    }
    reactive.state.incomings = incoming
    notify(reactive, changed = valuePersistency.ignitionRequiresReevaluation, new WriteableQueue[Task[FullMVTurn, Reactive[FullMVStruct]]] {
      override def pushBottom(element: Task[FullMVTurn, Reactive[FullMVStruct]]): Unit = {
        assert(element == Reevaluation(FullMVTurn.this, reactive), s"Ignition may only spawn Reevaluation(${FullMVTurn.this}, $reactive), but spawned $element")
        reevaluate(reactive, FullMVEngine.entryWorkQueue)
      }
    })
  }

  override private[rescala] def dynamicDependencyInteraction(reactive: Reactive[FullMVStruct]) = reactive.state.synchronizeDynamicAccess(this)
  override private[rescala] def before[P](pulsing: Pulsing[P, FullMVStruct]) = pulsing.state.staticBefore(this)
  override private[rescala] def after[P](pulsing: Pulsing[P, FullMVStruct]) = pulsing.state.staticNow(this)

  override def observe(f: () => Unit): Unit = f()

  override def toString: String = synchronized {
    "FullMVTurn(" + System.identityHashCode(this) + ", " + (state match {
      case 0 => "Initialized"
      case 1 => "Framing("+activeBranches+")"
      case 2 => "Executing("+activeBranches+")"
      case 3 => "WrapUp"
      case 4 => "Completed"
    })+ ")"
  }
}
