package rescala.fullmv

import rescala.engine.{EngineImpl, InitializationImpl, ValuePersistency}
import rescala.graph.{Pulsing, Reactive, Struct}
import rescala.fullmv.FramingBranchResult._
import rescala.fullmv.NotificationResultAction._
import NotificationOutAndSuccessorOperation._
import rescala.graph.ReevaluationResult.{Dynamic, Static}

import scala.util.Try

trait FullMVStruct extends Struct {
  override type State[P, S <: Struct] = NodeVersionHistory[P, FullMVTurn, Reactive[FullMVStruct]]
}

object FullMVEngine extends EngineImpl[FullMVStruct, FullMVTurn] {
  object sgt extends SerializationGraphTracking[FullMVTurn] {
    private var predecessors = Map[FullMVTurn, Set[FullMVTurn]]().withDefaultValue(Set())
    private var successors = Map[FullMVTurn, Set[FullMVTurn]]().withDefaultValue(Set())

    override def ensureOrder(defender: FullMVTurn, contender: FullMVTurn): OrderResult = synchronized {
      assert(defender.state > State.Initialized, s"$defender is not started and should thus not be involved in any operations")
      assert(contender.state > State.Initialized, s"$contender is not started and should thus not be involved in any operations")
      assert(contender.state < State.Completed, s"$contender cannot be a contender (already completed).")
      if(defender.state == State.Completed) {
        FirstFirst
      } else if (predecessors(defender)(contender)) {
        SecondFirst
      } else if (predecessors(contender)(defender)) {
        FirstFirst
      } else if(defender.state < contender.state) {
        establishOrder(contender, defender)
        SecondFirst
      } else {
        establishOrder(defender, contender)
        FirstFirst
      }
    }

    private def establishOrder(first: FullMVTurn, second: FullMVTurn): Unit = {
      val allAfter = successors(second) + second
      val allBefore = predecessors(first) + first
      for(succ <- allAfter) predecessors += succ -> (predecessors(succ) ++ allBefore)
      for(pred <- allBefore) successors += pred -> (successors(pred) ++ allAfter)
    }

    override def getOrder(found: FullMVTurn, searcher: FullMVTurn): PartialOrderResult = synchronized {
      assert(found.state > State.Initialized, s"$found is not started and should thus not be involved in any operations")
      assert(searcher.state > State.Initialized, s"$searcher is not started and should thus not be involved in any operations")
      assert(searcher.state < State.Completed, s"$searcher cannot be a searcher (already completed).")
      if(found.state == State.Completed) {
        FirstFirst
      } else if (searcher.state == State.Completed) {
        SecondFirst
      } else if (predecessors(found)(searcher)) {
        SecondFirst
      } else if (predecessors(searcher)(found)) {
        FirstFirst
      } else {
        Unordered
      }
    }

    def discard(turn: FullMVTurn): Unit = synchronized {
      assert(turn.state == State.Completed, s"Trying to discard incomplete $turn")
      for(succ <- successors(turn)) {
        val previousPredecessors = predecessors(succ)
        if(previousPredecessors.size == 1) {
          assert(previousPredecessors == Set(turn), s"predecessor tracking for $succ was inaccurate: should only contain $turn, but contained $previousPredecessors.")
          predecessors -= succ
        } else {
          val remainingPredecessors = predecessors(succ) - turn
          predecessors += succ -> remainingPredecessors
        }
      }
      successors -= turn
    }

    override def awaitAllPredecessorsState(turn: FullMVTurn, atLeast: State.Type): Unit = {
      // Note that each turn on which this suspends has the opportunity to add additional predecessors to this turn
      // transitively. We do, however, not need to repeatedly lookup the set of all predecessors, thereby ignoring these,
      // because the turn on which this suspended will hold the suspension until these new transitive predecessors have
      // reached the same stage first. Thus, looking up our own predecessors again after a suspension might reveal
      // additional predecessors, but they would all have reached the required state already.
      sgt.predecessors(turn).foreach {
        _.awaitState(atLeast)
      }
    }
  }

  override protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[FullMVTurn]): FullMVTurn = new FullMVTurn(sgt)
  override protected def executeInternal[I, R](turn: FullMVTurn, initialWrites: Traversable[Reactive], admissionPhase: () => I, wrapUpPhase: I => R): R = {
    // framing start
    turn.beginPhase(State.Framing, initialWrites.size)
    initialWrites.foreach(turn.incrementFrame)

    // framing completion
    // TODO this should be an await once we add in-turn parallelism
    turn.synchronized { assert(turn.activeBranches == 0, s"${turn.activeBranches} active branches remained after $turn framing phase") }
    sgt.awaitAllPredecessorsState(turn, State.Executing)

    // admission
    turn.beginPhase(State.Executing, initialWrites.size)
    val admissionResult = Try(admissionPhase())

    // propagation start
    initialWrites.foreach(turn.notify(_, changed = admissionResult.isSuccess, None))

    // propagation completion
    sgt.awaitAllPredecessorsState(turn, State.WrapUp)
    // TODO this should be an await once we add in-turn parallelism
    turn.synchronized { assert(turn.activeBranches == 0, s"${turn.activeBranches} active branches remained after $turn propagation phase") }

    // wrap-up
    turn.beginPhase(State.WrapUp, 0)

    val result = admissionResult.flatMap(i => Try { wrapUpPhase(i) })

    // turn completion
    sgt.awaitAllPredecessorsState(turn, State.Completed)
    turn.beginPhase(State.Completed, -1)
    sgt.discard(turn)

    // result
    result.get
  }

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
  var activeBranches: Int = 0
  def activeBranchDifferential(forState: State.Type, differential: Int): Unit = synchronized {
    assert(state == forState, s"$this received branch differential for wrong state $state")
    activeBranches += differential
  }

  def beginPhase(state: State.Type, initialActiveBranches: Int): Unit = synchronized {
    require(state > this.state, s"$this cannot progress backwards to phase $state.")
    assert(this.activeBranches == 0, s"$this still has active branches and thus cannot start phase $state!")
    this.activeBranches = initialActiveBranches
    this.state = state
    notifyAll()
  }

  def awaitState(atLeast: State.Type): Unit = synchronized {
    while(state < atLeast) {
      wait()
    }
  }

  def incrementFrame(node: Reactive[FullMVStruct]): Unit = {
    assert(state == State.Framing, s"$this cannot increment frame (requires framing phase)")
    processFramingBranching(node.state.incrementFrame(this))
  }

  def incrementSupersedeFrame(node: Reactive[FullMVStruct], superseded: FullMVTurn): Unit = {
    assert(state == State.Framing, s"$this cannot increment frame (requires framing phase)")
    assert(superseded.state == State.Framing, s"$superseded cannot have frame superseded (requires framing phase)")
    processFramingBranching(node.state.incrementSupersedeFrame(this, superseded))
  }

  private def processFramingBranching(result: FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]]): Unit = {
    result match {
      case FramingBranchEnd =>
        activeBranchDifferential(State.Framing, -1)
      case FramingBranchOut(out) =>
        if(out.size != 1) activeBranchDifferential(State.Framing, out.size - 1)
        out.foreach(incrementFrame)
      case FramingBranchOutSuperseding(out, supersede) =>
        if(out.size != 1) activeBranchDifferential(State.Framing, out.size - 1)
        out.foreach(d => incrementSupersedeFrame(d, supersede))
    }
  }

  def notify(node: Reactive[FullMVStruct], changed: Boolean, maybeFollowFrame: Option[FullMVTurn]): Unit = {
    assert(state == State.Executing, s"$this cannot receive notification (requires executing phase)")
    assert(!maybeFollowFrame.isDefined || maybeFollowFrame.get.state >= State.Framing, s"${maybeFollowFrame.get} cannot receive follow frame (requires at least Framing phase)")
    assert(!maybeFollowFrame.isDefined || maybeFollowFrame.get.state <= State.Executing, s"${maybeFollowFrame.get} cannot receive follow frame (requires at most Executing phase)")

    val notificationResultAction = node.state.notify(this, changed, maybeFollowFrame)
    // println(s"$this notified $node changed=$changed resulting in $notificationResultAction")
    notificationResultAction match {
      case GlitchFreeReadyButQueued =>
        activeBranchDifferential(State.Executing, -1)
      case ResolvedNonFirstFrameToUnchanged =>
        activeBranchDifferential(State.Executing, -1)
      case NotGlitchFreeReady =>
        activeBranchDifferential(State.Executing, -1)
      case GlitchFreeReady =>
        // no branch count change
        reevaluate(node)
      case outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]] =>
        processNotificationAndFollowOperation(node, changed = false, outAndSucc)
    }
  }

  // TODO optimize mutual tail-recursion with reevaluate?
  private def processNotificationAndFollowOperation(node: Reactive[FullMVStruct], changed: Boolean, outAndSucc: NotificationOutAndSuccessorOperation[FullMVTurn, Reactive[FullMVStruct]]): Unit = {
    outAndSucc match {
      case NoSuccessor(out) =>
        sendNotifications(out, changed, None)
      case FollowFraming(out, succTxn) =>
        sendNotifications(out, changed, Some(succTxn))
      case NextReevaluation(out, succTxn) =>
        sendNotifications(out, changed, Some(succTxn))
        succTxn.activeBranchDifferential(State.Executing, 1)
        succTxn.reevaluate(node)
    }
  }

  // TODO optimize mutual tail-recursion with processNotificationAndFollowOperation?
  def reevaluate(node: Reactive[FullMVStruct]): Unit = {
      val result = FullMVEngine.withTurn(this){ node.reevaluate(this) }
      result match {
        case Static(isChange, value) =>
          val out = node.state.reevOut(this, if(isChange) Some(value) else None)
          // println(s"$this reevaluated $node to $value, branching $out")
          processNotificationAndFollowOperation(node, isChange, out)
        case res @ Dynamic(isChange, value, deps) =>
          val diff = res.depDiff(node.state.incomings)
          diff.removed.foreach{ drop =>
            val (successorWrittenVersions, maybeFollowFrame) = drop.state.drop(this, node)
            node.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, -1)
          }
          // TODO after adding in-turn parallelism, nodes may be more complete here than they were during actual reevaluation, leading to missed glitches
          val anyPendingDependency = diff.added.foldLeft(false) { (anyPendingDependency, discover) =>
            val (successorWrittenVersions, maybeFollowFrame) = discover.state.discover(this, node)
            node.state.retrofitSinkFrames(successorWrittenVersions, maybeFollowFrame, 1)
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
            val out = node.state.reevOut(this, if (isChange) Some(value) else None)
            // println(s"$this reevaluated $node to $value, branching $out")
            processNotificationAndFollowOperation(node, isChange, out)
          }
      }
    }

  private def sendNotifications(out: Set[Reactive[FullMVStruct]], changed: Boolean, maybeFollowFrame: Option[FullMVTurn]): Unit = {
    if(out.size != 1) activeBranchDifferential(State.Executing,out.size - 1)
    out.foreach(notify(_, changed, maybeFollowFrame))
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
    notify(reactive, changed = valuePersistency.ignitionRequiresReevaluation, None)
  }

  override private[rescala] def dynamicDependencyInteraction(reactive: Reactive[FullMVStruct]) = reactive.state.synchronizeDynamicAccess(this)
  override private[rescala] def before[P](pulsing: Pulsing[P, FullMVStruct]) = pulsing.state.staticBefore(this)
  override private[rescala] def after[P](pulsing: Pulsing[P, FullMVStruct]) = pulsing.state.staticNow(this)

  override def observe(f: () => Unit): Unit = f()

  override def toString(): String = synchronized {
    "FullMVTurn(" + System.identityHashCode(this) + ", " + (state match {
      case 0 => "Initialized"
      case 1 => "Framing("+activeBranches+")"
      case 2 => "Executing("+activeBranches+")"
      case 3 => "WrapUp"
      case 4 => "Completed"
    })+ ")"
  }
}
